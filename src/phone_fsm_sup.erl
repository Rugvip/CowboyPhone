-module(phone_fsm_sup).
-export([start_link/0, init/1, stop/1, add_controller/2, remove_controller/2]).
-behaviour(supervisor).
-define(FSM, phone_fsm).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Fsm = {0, {?FSM, start_link, []}, temporary, 5000, worker, [?FSM]},
    {ok, {{simple_one_for_one, 10, 10}, [Fsm]}}.

stop(Pid) -> exit(Pid, kill), ok.

add_controller(SupPid, Number) ->
    {ok, Pid} = supervisor:start_child(SupPid,  [Number]),
    {ok, Pid}.

remove_controller(SupPid, Number) ->
    case hlr:lookup_id(Number) of
        {ok, Pid} -> ok = supervisor:terminate_child(SupPid, Pid);
        _ -> ok
    end,
    ok.
