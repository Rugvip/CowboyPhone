-module(socket_phone).
-behaviour(cowboy_websocket_handler).

% Behaviour cowboy_http_handler
-export([init/3]).

% Behaviour cowboy_http_websocket_handler
-export([websocket_init/3, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    io:format("DSADSASD~n"),
    {upgrade, protocol, cowboy_websocket}.

% Called for every new websocket connection.
websocket_init(_Any, Req, []) ->
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    io:format("Received: ~p", [Msg]),
    {reply,
        {text, << "Responding to ", Msg/binary >>},
        Req, State, hibernate
    };

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(_Any, Req, State) ->
    io:format("Other websocket message~n"),
    {ok, Req, State}.

% Other messages from the system are handled here.
websocket_info(_Info, Req, State) ->
    io:format("Other system message~n"),
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format("Websocket terminated~n"),
    ok.
