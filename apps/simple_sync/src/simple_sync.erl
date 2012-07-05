-module(simple_sync).
-behaviour(gen_fsm).
-export([start_link/1, sync/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/3]).
-record(state, {from}).

sync(FSMPid, Sync)->
    gen_fsm:sync_send_event(FSMPid, {sync, Sync}).

start_link(InitData) ->
    gen_fsm:start_link(?MODULE, [InitData], []).

init([InitData])->
    % Pre-default state - unprepared. Prepare FSM here!    
    % Sync URL, sync account info, local and remote database URI's,.    
    % conflict resolution policy details,.    
    % sync_mode (slow-sync, two-way-sync, one-way-sync)    
    % operation mode, add source - more source db    
    % protocol version    
    % logging    
    % Session id
    error_logger:info_msg("Init server with:~n", [InitData]),
    {ok, ready, #state{}}.

ready({sync, Sync},  From, State)->
    error_logger:info_msg("FSM is ready, sync!", [Sync]),
    Status = {status, lists:map(fun dispatch_cmd/1, [{Name, Cmd} || {Name, Cmd} <- Sync])},
    {reply, Status, ready, State#state{from=From}}.

dispatch_cmd({add, Cmd = [{cmd_id, CmdId}|_]})->
    error_logger:info_msg("Dispatch Add command:", CmdId),
    Status = add,
    {Status, [CmdId]};
dispatch_cmd(_)->
    {ok}.

handle_event(_Event, StateName, State)->
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State)->
    {reply, ok, StateName, State}.
handle_info(_Info, StateName, State)->
    {next_state, StateName, State}.
terminate(_Reason, _StateName, _State)->
    ok.
code_change(_OldVsn, StateName, State, _Extra)->
    {ok, StateName, State}.