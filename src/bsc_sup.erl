-module(bsc_sup).
-export([start_link/0, init/1, add_controller/1, remove_controller/1]).
-behaviour(supervisor).
-define(HLR, hlr).
-define(FSM_SUP, phone_fsm_sup).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Hlr = {?HLR, {?HLR, start_link, []}, permanent, 5000, worker, [?HLR, hlr_db]},
    FsmSup = {?FSM_SUP, {?FSM_SUP, start_link, []}, permanent, infinity, worker, [?FSM_SUP]},
    {ok, {{one_for_one, 10, 10}, [Hlr, FsmSup]}}.

add_controller(Number) ->
    phone_fsm_sup:add_controller(?FSM_SUP, Number).

remove_controller(Number) ->
    phone_fsm_sup:remove_controller(?FSM_SUP, Number),
    {ok, self()}.
