-module(simple_sync).
-behaviour(gen_fsm).
-export([start_link/1, message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/3]).
-record(state, {}).

message(FSMPid, Msg)->
    gen_fsm:sync_send_event(FSMPid, {msg, Msg}).

start_link(InitData) ->
    gen_fsm:start_link(?MODULE, [InitData], []).

init([InitData])->
    % Sync URL, sync account info, local and remote database URI's,.    
    % conflict resolution policy details,.    
    % sync_mode (slow-sync, two-way-sync, one-way-sync)    
    % operation mode, add source - more source db    
    % protocol version, logging, Session id
    {ok, ready, #state{}}.

ready({msg, Msg}, From, State)->
    error_logger:info_msg("FSM is ready!", Msg),
    Status = [{'Status', X} || X <- lists:map(fun dispatch_cmd/1, [{Name, Cmd} || {Name, Cmd} <- Msg])],
    error_logger:info_msg("Status:", Status),
    {reply, Status, ready, State}.

dispatch_cmd({Name='Add', Cmd = [{'CmdID', CmdID}|_]})->
    error_logger:info_msg("Dispatch (~p,~p) command", [Name, CmdID]),
    status(Name, CmdID);
dispatch_cmd({Name='Sync', Cmd=[{'CmdID', CmdID}|_]})->
    error_logger:info_msg("Dispatch (~p,~p) command", [Name, CmdID]),
    Status = status(Name, CmdID),
    Status;
dispatch_cmd(_)->
    error_logger:info_msg("Dispatch -some- cmd"),
    [].

status(CmdName, CmdID)->
    [{'CmdID', ["1"]}, %Unique cmd id
    {'MsgRef', ["1"]}, %Unique msg id
    {'CmdRef', [CmdID]},
    {'Cmd', [atom_to_list(CmdName)]},
    {'SourceRef', ["1212"]},%Source address specified in the command
    {'Data',["208"]}].

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