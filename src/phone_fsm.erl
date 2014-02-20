-module(phone_fsm).

-export([start_link/1, stop/1, connect/1, disconnect/1, action/2]).
-export([init/1, idle/2, calling/2, receiving/2, connected/2, handle_info/3, terminate/3, handle_sync_event/4]).

-record(st, {phone, remote, remote_mon, phone_mon}).

start_link(PhoneNumber) ->
    gen_fsm:start_link(?MODULE, PhoneNumber, []).
    % gen_fsm:start_link(?MODULE, PhoneNumber, [{debug, [trace]}]).

stop(FsmPid) -> gen_fsm:sync_send_all_state_event(FsmPid, stop).
connect(FsmPid) -> gen_fsm:sync_send_all_state_event(FsmPid, connect).
disconnect(FsmPid) -> gen_fsm:sync_send_all_state_event(FsmPid, disconnect).

send_to_local(FsmPid, Action) -> gen_fsm:send_event(FsmPid, {local, Action}).
send_to_remote(Pid, Msg) -> gen_fsm:send_event(Pid, {remote, self(), Msg}).

action(FsmPid, Action) -> send_to_local(FsmPid, Action).

busy(RemotePid)    -> send_to_remote(RemotePid, busy).
reject(RemotePid)  -> send_to_remote(RemotePid, reject).
accept(RemotePid)  -> send_to_remote(RemotePid, accept).
hangup(RemotePid)  -> send_to_remote(RemotePid, hangup).
inbound(RemotePid) -> send_to_remote(RemotePid, inbound).
data(RemotePid, Data) -> send_to_remote(RemotePid, {data, Data}).

reply(Msg, NextState, #st{phone = Phone} = State) ->
    phone:reply(Phone, Msg),
    switch_state(NextState, Msg, State).

% gen_fsm

set_remote(State, Remote) ->
    Ref = monitor(process, Remote),
    State#st{remote = Remote, remote_mon = Ref}.

unset_remote(State) ->
    demonitor(State#st.remote_mon),
    State#st{remote_mon = undefined, remote = undefined}.

switch_state(NextState, Action, #st{phone = Phone} = State) ->
    Phone ! {switch_state, NextState, Action},
    {next_state, NextState, State}.
switch_state(NextState, State) -> switch_state(NextState, undefined, State).

% gen_fsm callbacks

%      ######  ########
%     ##    ## ##     ##
%     ##       ##     ##
%     ##       ########
%     ##       ##     ##
%     ##    ## ##     ##
%      ######  ########

init(PhoneNumber) ->
    process_flag(trap_exit, true),
    hlr:attach(PhoneNumber),
    {ok, idle, #st{}}.


idle({local, {outbound, Number}}, State) ->
    case hlr:lookup_id(Number) of
        {error, invalid} -> {next_state, idle, State};
        {ok, Remote} ->
            inbound(Remote),
            switch_state(calling, set_remote(State, Remote))
    end;

idle({remote, From, inbound}, State) ->
    {ok, Number} = hlr:lookup_phone(From),
    reply({inbound, Number}, receiving, set_remote(State, From));

idle(_, State) ->
    {next_state, idle, State}.


calling({remote, From, inbound}, State) ->
    busy(From), {next_state, calling, State};

calling({remote, From, accept}, #st{remote = From} = State) ->

    reply(accept, connected, State);

calling({remote, From, reject}, #st{remote = From} = State) ->
    reply(reject, idle, unset_remote(State));

calling({remote, From, busy}, #st{remote = From} = State) ->
    reply(busy, idle, unset_remote(State));

calling({local, hangup}, State) ->
    hangup(State#st.remote), switch_state(idle, unset_remote(State));

calling(_, State) ->
    {next_state, calling, State}.


receiving({remote, From, inbound}, State) ->
    busy(From), {next_state, receiving, State};

receiving({remote, From, hangup}, #st{remote = From} = State) ->
    reply(hangup, idle, unset_remote(State));

receiving({local, accept}, State) ->
    accept(State#st.remote), switch_state(connected, State);

receiving({local, reject}, State) ->
    reject(State#st.remote), switch_state(idle, unset_remote(State));

receiving(_, State) ->
    {next_state, receiving, State}.


connected({remote, From, inbound}, State) ->
    busy(From), {next_state, connected, State};

connected({remote, From, hangup}, #st{remote = From} = State) ->
    reply(hangup, idle, unset_remote(State));

connected({local, hangup}, State) ->
    hangup(State#st.remote), switch_state(idle, unset_remote(State));

connected({local, {data, Data}}, State) ->
    data(State#st.remote, Data),
    {next_state, connected, State};

connected({remote, _, {data, _} = Data}, State) ->
    State#st.phone ! Data,
    {next_state, connected, State};

connected(_, State) ->
    {next_state, connected, State}.


handle_info({'DOWN', Ref, process, _, _}, _StateName, #st{remote_mon = Ref} = State) ->
    switch_state(idle, remote_down, State#st{remote_mon = undefined, remote = undefined});
handle_info({'DOWN', Ref, process, _, _}, _StateName, #st{phone_mon = Ref} = State) ->
    case State#st.remote_mon of
        undefined -> ok;
        Mon -> demonitor(Mon)
    end,
    {next_state, idle, State#st{
        phone_mon = undefined, phone = undefined,
        remote_mon = undefined, remote = undefined
    }};
handle_info(Info, StateName, State) ->
    io:format("Got info: ~p~n", [Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    io:format("FSM terminating, detach~n"),
    hlr:detach(),
    ok.

handle_sync_event(connect, {Pid, _}, StateName, State) ->
    case State#st.phone of
        undefined ->
            Mon = monitor(process, Pid),
            {reply, ok, StateName, State#st{phone = Pid, phone_mon = Mon}};
        _ -> {reply, busy, StateName, State}
    end;
handle_sync_event(disconnect, {Pid, _}, StateName, #st{phone = Pid} = State) ->
    demonitor(State#st.phone_mon),
    {reply, ok, StateName, State#st{phone_mon = undefined, phone = undefined}};
handle_sync_event(stop, _, _StateName, State) ->
    {stop, normal, ok, State#st{phone = undefined}}.

%% Phone -> Controller
%   {local, {outbound, Number}} - make outbound call
%   {local, accept} - accept incoming call
%   {local, reject} - reject incoming call
%   {local, hangup} - hangup outbound or ongoing call

%% Controller -> Controller
%   {remote, Pid, busy}} - send is busy
%   {remote, Pid, reject}} - sender rejects remotes call
%   {remote, Pid, accept}} - sender accepts remotes call
%   {remote, Pid, hangup}} - sender hangs up
%   {remote, Pid, inbound}} - sender initiates call

%% Controller -> Phone
%   {inbound, PhoneNumber} - an inbound call from PhoneNumber
%   accept - an outbound call has been accepted
%   invalid - an outbound call to an invalid number
%   reject - an outbound call has been rejected
%   busy - an outbound call was to a busy phone
%   hangup - an ongoing call has been hung up
