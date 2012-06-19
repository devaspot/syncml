-module(simple_sync).
-behaviour(gen_fsm).
-export([start_link/0, message/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([build_message/3]).
-record(state, {from}).

message(FSMPid, Msg)->
    gen_fsm:sync_send_event(FSMPid, {msg, Msg}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

init([])->
    {ok, build_message, #state{}}.

build_message({msg, Msg}, From, State)->
    error_logger:info_msg("FSM in build_message, receive 'msg'", [Msg]),
    {reply, "reply-ok", build_message, State#state{from=From}}.

%build_command(E=#xmlElement{})->    
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