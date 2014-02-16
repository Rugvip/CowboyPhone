-module(phone_fsm).

-export([start_link/1, stop/1, connect/1, disconnect/1, action/2]).
-export([init/1, idle/2, calling/2, receiving/2, connected/2, handle_info/3, terminate/3, handle_sync_event/4]).

-record(st, {phone, remote}).


start_link(PhoneNumber) ->
    gen_fsm:start_link(?MODULE, PhoneNumber, [{debug, [trace]}]).

stop(FsmPid) -> gen_fsm:sync_send_all_state_event(FsmPid, stop).
connect(FsmPid) -> gen_fsm:sync_send_all_state_event(FsmPid, connect).
disconnect(FsmPid) -> gen_fsm:sync_send_all_state_event(FsmPid, disconnect).

send_to_local(FsmPid, Action) -> gen_fsm:send_event(FsmPid, {local, Action}).
send_to_remote(Pid, Msg) -> gen_fsm:send_event(Pid, {remote, self(), Msg}).
reply(Msg, NextState, #st{phone = Phone} = State) ->
    gen_server:cast(Phone, {reply, Msg}),
    {next_state, NextState, State}.

action(FsmPid, {outbound, Number}) -> send_to_local(FsmPid, {outbound, Number});
action(FsmPid, accept)             -> send_to_local(FsmPid, accept);
action(FsmPid, reject)             -> send_to_local(FsmPid, reject);
action(FsmPid, hangup)             -> send_to_local(FsmPid, hangup).

busy(RemotePid)    -> send_to_remote(RemotePid, busy).
reject(RemotePid)  -> send_to_remote(RemotePid, reject).
accept(RemotePid)  -> send_to_remote(RemotePid, accept).
hangup(RemotePid)  -> send_to_remote(RemotePid, hangup).
inbound(RemotePid) -> send_to_remote(RemotePid, inbound).

% gen_fsm

init(PhoneNumber) ->
    hlr:attach(PhoneNumber),
    {ok, idle, #st{}}.

reset_remote(State) -> State#st{remote = undefined}.

idle({local, {outbound, Number}}, State) ->
    {ok, Remote} = hlr:lookup_id(Number),
    inbound(Remote),
    {next_state, calling, State#st{remote = Remote}};
idle({remote, From, inbound}, State) ->
    {ok, Number} = hlr:lookup_phone(From),
    reply({inbound, Number}, receiving, State#st{remote = From});
idle(_, State) ->
    {next_state, idle, State}.

calling({remote, From, inbound}, State) ->
    busy(From), {next_state, calling, State};
calling({remote, From, accept}, #st{remote = From} = State) ->
    reply(accept, connected, State);
calling({remote, From, rejected}, #st{remote = From} = State) ->
    reply(reject, idle, reset_remote(State));
calling({remote, From, busy}, #st{remote = From} = State) ->
    reply(busy, idle, reset_remote(State));
calling({local, hangup}, State) ->
    hangup(State#st.remote), {next_state, idle, reset_remote(State)};
calling(_, State) ->
    {next_state, calling, State}.

receiving({remote, From, inbound}, State) ->
    busy(From), {next_state, receiving, State};
receiving({remote, From, hangup}, #st{remote = From} = State) ->
    reply(hangup, idle, reset_remote(State));
receiving({local, accept}, State) ->
    accept(State#st.remote), {next_state, connected, State};
receiving({local, reject}, State) ->
    reject(State#st.remote), {next_state, idle, reset_remote(State)};
receiving(_, State) ->
    {next_state, receiving, State}.

connected({remote, From, inbound}, State) ->
    busy(From), {next_state, connected, State};
connected({remote, From, hangup}, #st{remote = From} = State) ->
    reply(hangup, idle, reset_remote(State));
connected({local, hangup}, State) ->
    hangup(State#st.remote), {next_state, idle, reset_remote(State)};
connected(_, State) ->
    {next_state, connected, State}.

handle_info(Info, StateName, State) ->
    io:format("Got info: ~p~n", [Info]),
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    hlr:detach(),
    ok.

handle_sync_event(connect, {Pid, _}, StateName, State) ->
    {reply, ok, StateName, State#st{phone = Pid}};
handle_sync_event(disconnect, {Pid, _}, StateName, #st{phone = Pid} = State) ->
    {reply, ok, StateName, State#st{phone = undefined}};
handle_sync_event(stop, _, StateName, State) ->
    {stop, normal, StateName, State#st{phone = undefined}}.

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
