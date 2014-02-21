-module(phone_socket).
-behaviour(cowboy_websocket_handler).

% Behaviour cowboy_http_handler
-export([init/3]).

% Behaviour cowboy_http_websocket_handler
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

-record(st, {fsm, fsm_mon}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

% Called for every new websocket connection.
websocket_init(_Any, Req, []) ->
    {NumberBin, Req2} = cowboy_req:binding(number, Req, ""),
    Number = binary_to_list(NumberBin),
    io:format("Num: ~p~n", [Number]),
    {ok, Fsm} = hlr:lookup_id(Number),
    io:format("THEN WHO WAS PHONE? ~p~n", [Fsm]),
    self() ! case phone_fsm:connect(Fsm) of
        ok -> io:format("CONNECT!~n"), {init, ok};
        busy -> io:format("BUSY GUSY~n"), {init, busy}
    end,
    Req3 = cowboy_req:compact(Req2),
    {ok, Req3, #st{fsm = Fsm}, hibernate}.

process_message({struct, [{<<"action">>, <<"accept">>}]}, State) ->
    action(accept, State), ok;
process_message({struct, [{<<"action">>, <<"reject">>}]}, State) ->
    action(reject, State), ok;
process_message({struct, [{<<"action">>, <<"hangup">>}]}, State) ->
    action(hangup, State), ok;
process_message({struct, [{<<"action">>, <<"disconnect">>}]}, _State) ->
    stop;
process_message({struct, [{<<"action">>, {struct, [{<<"call">>, Number}]}}]}, State) ->
    action({outbound, binary_to_list(Number)}, State), ok;
process_message({struct, [{<<"data">>, Data}]}, State) ->
    action({data, Data}, State), ok;
process_message(_, _) ->
    json([{error, 'bad request'}, {status, '400'}]).

action(Action, #st{fsm = Fsm}) ->
    io:format("Sending ~p to ~p~n", [Action, Fsm]),
    phone_fsm:action(Fsm, Action).

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    io:format("Received: ~p~n", [Msg]),
    case process_message(mochijson2:decode(Msg), State) of
        stop -> {shutdown, Req, State};
        ok -> {ok, Req, State, hibernate};
        Reply -> {reply, {text, Reply}, Req, State, hibernate}
    end;

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(_Any, Req, State) ->
    io:format("Other websocket message~n"),
    {ok, Req, State}.

% Other messages from the system are handled here.
websocket_info(stop, Req, State) -> {shutdown, Req, State};
websocket_info({init, ok}, Req, #st{fsm = Fsm} = State) ->
    Ref = monitor(process, Fsm),
    {ok, Req, State#st{fsm_mon = Ref}};
    % reply(Req, State#st{fsm_mon = Ref}, idle, undefined);
websocket_info({init, busy}, Req, State) ->
    reply(Req, State, none, busy);
websocket_info({switch_state, NextState, Action}, Req, State) ->
    reply(Req, State, NextState, Action);
websocket_info({data, Data}, Req, State) ->
    {reply, {text, mochijson2:encode({struct, [{<<"data">>, Data}]})}, Req, State};
websocket_info({'DOWN', Ref, process, _, _}, Req, #st{fsm_mon = Ref} = State) ->
    self() ! stop,
    reply(Req, State, undefined, deleted);
websocket_info(Msg, Req, State) ->
    io:format("Other system message: ~p~n", [Msg]),
    {ok, Req, State, hibernate}.

websocket_terminate(Reason, _Req, _State) ->
    io:format("Websocket terminated: ~p~n", [Reason]),
    ok.
reply(Req, State, StateName, {inbound, Number}) ->
    {reply, {text, mochijson2:encode({
        struct, [{<<"state">>, atom_to_binary(StateName, utf8)},
        {<<"action">>, {struct, [{<<"inbound">>, list_to_binary(Number)}]}}]
    })}, Req, State};
reply(Req, State, StateName, Action) -> {reply, {text, json([
        {state, StateName},
        {action, Action}
    ])}, Req, State}.

json(List) ->
    KeyVals = [{atom_to_binary(Key, utf8), atom_to_binary(Value, utf8)}
        || {Key, Value} <- List, Value =/= undefined],
    mochijson2:encode({struct, KeyVals}).
