-module(hlr).
-behaviour(gen_server).

-export([start_link/0, attach/1, detach/0, lookup_id/1, lookup_phone/1, list_numbers/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(st, {db}).
-define(DB_IMPL, hlr_db).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []), {ok, self()}.

attach(Number) when is_list(Number) ->
    gen_server:call({global, ?MODULE}, {attach, Number}).

detach() ->
    gen_server:call({global, ?MODULE}, detach).

lookup_id(Number) when is_list(Number) ->
    gen_server:call({global, ?MODULE}, {lookup_id, Number}).

lookup_phone(Pid) when is_pid(Pid) ->
    gen_server:call({global, ?MODULE}, {lookup_phone, Pid}).

list_numbers() -> gen_server:call({global, ?MODULE}, list_numbers).

% gen_server

init([]) ->
    State = #st{db = ?DB_IMPL:new([])},
    {ok, State}.

handle_call({attach, Number}, {Pid, _}, #st{db = Db} = State) ->
    NewDb = ?DB_IMPL:attach(Db, Pid, Number),
    {reply, ok, State#st{db = NewDb}};

handle_call(detach, {Pid, _}, #st{db = Db} = State) ->
    NewDb = ?DB_IMPL:detach(Db, Pid),
    {reply, ok, State#st{db = NewDb}};

handle_call({lookup_id, Number}, _, #st{db = Db} = State) ->
    {reply, ?DB_IMPL:lookup_id(Db, Number), State};

handle_call({lookup_phone, Pid}, _, #st{db = Db} = State) ->
    {reply, ?DB_IMPL:lookup_phone(Db, Pid), State};

handle_call(list_numbers, _, #st{db = Db} = State) ->
    {reply, ?DB_IMPL:list_numbers(Db), State}.


handle_cast(Request, State) ->
    io:format("handle_cast: ~p~n", [[Request, State]]),
    ok.

handle_info(Info, State) ->
    io:format("handle_info: ~p~n", [[Info, State]]),
    ok.

terminate(Reason, State) ->
    io:format("terminate: ~p~n", [[Reason, State]]),
    ok.

code_change(OldVsn, State, Extra) ->
    io:format("code_change: ~p~n", [[OldVsn, State, Extra]]),
    {ok, State}.
