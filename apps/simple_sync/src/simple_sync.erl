-module(simple_sync).
-behaviour(gen_fsm).
-export([start_link/0, start_link/1, message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([ready/3]).
-record(state, {from}).

message(FSMPid, Msg)->
    gen_fsm:sync_send_event(FSMPid, {msg, Msg}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

start_link(InitData) ->
    gen_fsm:start_link(?MODULE, [InitData], []).

init([])->
    % Pre-default state - unprepared. Prepare FSM here!    
    % Prepare FSM to sync.    
    % Sync URL, sync account info, local and remote database URI's,.    
    % conflict resolution policy details,.    
    % sync_mode (slow-sync, two-way-sync, one-way-sync)    
    % operation mode, add source - more source db    
    % protocol version    
    % logging    
    %.    
    % Session id
    {ok, ready, #state{}}.

% FSM States
% local_init
% - Performed SyncML initialization phase. Phase include authentication, exchange of device info sync mode negotiations.
% remote_init
% - When sync agent in local_init state, it is receiving handshaking info from remote side about SyncML init phase.
% receiving items
% - When sync agent is in receiving items state, it is receiving changes detected by remote side.
% sending items  - if server modification are needed to send
% - sending detected changes in local db to remote side 
% receiving mappings 
% - receiving the remote UID mappings of the changes sent to remote side.
% finalizing - 
% - it is waiting for map asknowledgement package from remote side. After this package has been received, the sync agent will finalize 
% sync session by storing the sync anchors 
% sync_finished
% % initiated sync session has completed succesfully
%INVALID_CONFIGURATIONSyncAgentConfig object passed SyncAgent is invalid, for example it might not have proper TransportConnectionData object or it has no sync targets specified.INTERNAL_ERRORInternal error has occurred during processing of operation.ABORTEDSync session was aborted by user or due to fatal error received in protocol response.SUSPENDINGUser has called suspendSync() method and SyncAgent is currenly negotiating with server about suspending the session.SUSPENDEDSyncAgent has finished suspending the session.CONNECTION_ERRORTransport layer has lost connection to remote side during session.INVALID_SYNCML_MESSAGEReceived response could not be parsed as the response was empty or contained invalid or unrecognized data.

ready({msg, Msg}, From, State)->
    error_logger:info_msg("FSM is ready, receive", [Msg]),
    {reply, ok, ready, State#state{from=From}}.

%% - Target and Source -
% A relative URI can be specified, if the proper absolute URI can be constructed.    
% from prefixing the respective Target or Source value from the SyncHdr to this relative URI.    
% MUST use LocURI element. CGI script parameters can be appended to the URI to perform selection filtering on the server target.
%    error_logger:info_msg("bulding command...", [E#xmlElement.content]).
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