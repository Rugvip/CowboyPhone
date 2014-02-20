%%%-------------------------------------------------------------------
%%% @author Joel Denke, Mats Maatson
%%% @copyright (C) 2014, KTH
%%% @doc
%%%    Phone test simulator
%%% @end
%%% Created : 10. feb 2014 07:30
%%%-------------------------------------------------------------------
-module(phone_test).

%% API
-export([phone_fsm_all/0, phone_fsm_task/0, phone_simulator/0, phone_fsm_sup/0, bsc_sup/0, phone_fsm_call_myself/0,
all/0, all_tasks/0, hlr/0]).

all() ->
  all_tasks(),
  phone_simulator(),
  ok.

all_tasks() ->
  hlr(),
  phone_fsm_all(),
  phone_fsm_task(),
  phone_fsm_sup(),
  bsc_sup(),
  ok.

hlr() ->
  io:format("Running task test: 3.1 HLR\n"),
  PhoneNo = "123",
  start_hlr(),

  ok = start_links(phone_fsm, [PhoneNo]),% Need to call hlr:attach(PhoneNumber) from a process
  case hlr:lookup_id(PhoneNo) of
     {ok, FsmPid} ->
       io:format("Found Pid ~p with phone number ~s \n", [FsmPid, PhoneNo]),

       case hlr:lookup_phone(FsmPid) of
         {ok, Number} -> io:format("Found phone number ~s from Pid ~p \n", [Number, FsmPid]);
         {error, invalid} -> io:format("Error: phone number ~s was not found \n", [PhoneNo])
       end,
       io:format("Stop Phone fsm Pid ~p\n", [FsmPid]),
       ok = phone_fsm:stop(FsmPid); % Need to call hlr:detach() from a process

     {error, invalid} -> io:format("Error: No pid with phone number ~s was found \n", [PhoneNo])
  end,

  timer:sleep(1000),

  case hlr:lookup_id(PhoneNo) of
    {ok, Pid} -> io:format("Found Pid ~p with phone number ~s", [Pid, PhoneNo]);
    {error, invalid} -> io:format("Success: No pid with phone number ~s was found \n", [PhoneNo])
  end,
  ok.

bsc_sup() ->
  io:format("Running task test: 3.3.2 bsc_sup\n"),
  io:format("Start Basestation Supervisor\n", []),
  {ok, Sup} = bsc_sup:start_link(),

  io:format("Add Phone FSM controllers\n", []),
  ok = mod_sup_controllers(bsc_sup, {bsc, add}, Sup, ["123", "124", "125"]),

  Pid = whereis(phone_fsm_sup),

  io:format("Current controllers ~p \n", [supervisor:which_children(Pid)]),
  timer:sleep(1000),

  ok = mod_sup_controllers(bsc_sup, {bsc, remove}, Sup, ["123", "124", "125"]),
  io:format("Current controllers ~p \n", [supervisor:which_children(Pid)]),
  [] = supervisor:which_children(Pid),
  ok.

phone_fsm_sup() ->
  io:format("Running task test: 3.3.1 phone_fsm_sup\n"),
  start_hlr(),

  io:format("Start Phone FSM Supervisor\n", []),
  {ok, Sup} = phone_fsm_sup:start_link(),

  io:format("Add Phone FSM controllers\n", []),
  ok = mod_sup_controllers(phone_fsm_sup, {fsm, add}, Sup, ["123", "124", "125"]),

  io:format("Current controllers ~p \n", [supervisor:which_children(Sup)]),
  timer:sleep(1000),

  ok = mod_sup_controllers(phone_fsm_sup, {fsm, remove}, Sup, ["123", "124", "125"]),

  io:format("Stop Phone FSM Supervisor\n", []),
  ok = phone_fsm_sup:stop(Sup),

  timer:sleep(2000),
  ok = lookup_phones(["123", "124", "125"]),

  ok.

phone_simulator() ->
  io:format("Roberts simulator\n"),
  start_hlr(),

  io:format("Start BSC Simulator link\n", []),
  {ok,Sim} = bsc_sim:start_link(10000),
  io:format("Start BSC Simulator with 1 second between each delay\n", []),
  ok = bsc_sim:start_run(Sim, 10),
  timer:sleep(5000),
  io:format("Got status: ~p \n", [bsc_sim:get_status(Sim)]),
  timer:sleep(1000),
  io:format("Got status: ~p \n", [bsc_sim:get_status(Sim)]),
  io:format("Stop BSC Simulator\n", []),
  timer:sleep(1000),
  ok = bsc_sim:stop_run(Sim),
  timer:sleep(5000),
  io:format("Got status: ~p \n", [bsc_sim:get_status(Sim)]),
  ok.

phone_fsm_call_myself() ->
  start_hlr(),

  ok = start_links(phone_fsm, ["123"]),
  {ok, P123} = phone:start_link("123"),
  ok = run_phone_actions([
    {P123, {call, "123"}}
  ]),

  phone:stop(P123),
  ok = stop_links(phone_fsm, ["123"]),
  ok.

phone_fsm_all() ->
  io:format("Running phone FSM test: All actions\n"),
  start_hlr(),

  ok = start_links(phone_fsm, ["123", "124", "125"]),

  {ok, P123} = phone:start_link("123"),
  {ok, P124} = phone:start_link("124"),
  {ok, P125} = phone:start_link("125"),

  ok = run_phone_actions([
    {P123, {call, "124"}},
    {P124, reject},
    {P125, {call, "123"}},
    {P123, accept},
    {P123, hangup},
    {P124, {call, "125"}},
    {P125, accept},
    {P124, hangup}
  ]),

  phone:stop(P123),
  phone:stop(P124),
  phone:stop(P125),

  ok = stop_links(phone_fsm, ["123", "124", "125"]),

  ok.

phone_fsm_task() ->
  io:format("Running task test: 3.2 assignment test\n"),
  start_hlr(),

  ok = start_links(phone_fsm, ["123", "124", "125"]),

  {ok, P123} = phone:start_link("123"),
  {ok, P124} = phone:start_link("124"),
  {ok, P125} = phone:start_link("125"),

  ok = run_phone_actions([
    {P123, {call, "124"}},
    {P124, accept},
    {P125, {call, "123"}},
    {P125, {call, "124"}}
  ]),

  phone:stop(P123),
  phone:stop(P124),
  phone:stop(P125),

  ok = stop_links(phone_fsm, ["123", "124", "125"]),

  ok.

lookup_phones(Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      case hlr:lookup_id(K) of
        {ok, Pid} -> io:format("Number ~s was found with Pid ~p in HLR ~n", [K, Pid]);
        {error, invalid} -> io:format("Number ~s was not found in HLR ~n", [K])
      end,
      ok
    end,
    ok, Numbers).

start_hlr() ->
  io:format("Start HLR\n", []),
  case hlr:start_link() of
    {ok, HLR} -> io:format("OK, HLR link was created with pid ~p \n", [HLR]);
    {error, {_, HLR}} -> io:format("OK, HLR link already started with pid ~p \n", [HLR]);
    {error, Message} -> io:format("Error, HLR failed start link, reason: ~p \n", [Message])
  end.

mod_sup_controllers(Mod, Mode, SupPid,  Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      case Mode of
        {fsm, add}     ->
          {ok, Pid} = Mod:add_controller(SupPid, K),
          io:format("Module ~s: Added and started process ~p with number ~s ~n", [Mod, Pid, K]);
        {fsm, remove}  ->
          ok = Mod:remove_controller(SupPid, K),
          io:format("Supervisor ~p: Removed phone ~s ~n", [SupPid, K]);
        {bsc, add}     ->
          {ok, Pid} = Mod:add_controller(K),
          io:format("Module ~s: Added and started process ~p with number ~s ~n", [Mod, Pid, K]);
        {bsc, remove}  ->
          {ok, Pid} = Mod:remove_controller(K),
          io:format("Supervisor ~p: Removed phone ~s from fsm supervisor ~p ~n", [SupPid, K, Pid])
      end,
      ok
    end,
    ok, Numbers).

start_links(Mod, Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      {ok, Pid} = Mod:start_link(K),
      io:format("Module ~s: Started process ~p with number ~s ~n", [Mod, Pid, K]),
      ok
    end,
  ok, Numbers).

stop_links(Mod, Numbers) ->
  lists:foldl(
    fun (K, _D) ->
      case hlr:lookup_id(K) of
        {ok, Pid} -> Mod:stop(Pid);
        {error, invalid} -> io:format("Pid with phone number ~s was already stopped ~n", [K])
      end,
      ok
    end,
    ok, Numbers).

run_phone_actions(Actions) ->
  lists:foldl(
    fun ({Pid, Action}, ok) ->
      io:format("Running phone action: ~p from pid ~p ~n", [Action, Pid]),
      phone:action(Pid, Action),
      case Action of
        {call, _} -> timer:sleep(3000);
        _Other     -> timer:sleep(2000)
      end,
      ok
       % Wait between actions
    end,
    ok, Actions).
