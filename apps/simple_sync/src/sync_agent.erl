-module(sync_agent).
-behaviour(gen_server).
-export([start_link/1, message/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-record(state, {fsm_pid}).
-include_lib("xmerl/include/xmerl.hrl").

message(Msg)->
    {_, InitData} = lists:keyfind("init_data", 1, Msg),
    {ok, Pid} = sync_agent_sup:start_child(InitData),
    gen_server:call(Pid, {msg, Msg}).

% Delegate tasks not related to Sync session to SyncML commands handler 
% SyncML command objects can transform its data content to XML
% -sendNextMessage - use response generator to send next message based on the messages in the queue
% pakage queue, messages
% -saveSession - save the session info, changelog, mapping, anchors etc.
% -finishSync

start_link(Msg)->
    gen_server:start_link(?MODULE, [Msg], []).

init([InitData])->
    {_, SessionID} = lists:keyfind("session_id", 1, InitData),
    State = #state{fsm_pid = fsm_pid(SessionID)},
    {ok, State}.

handle_call({msg, Msg}, _From, State )->
    Result = simple_sync:message(State#state.fsm_pid, Msg),
    error_logger:info_msg("Engine call result:", [Result]),
    {stop, normal, Result, State}.

handle_cast(_Call, State)->
    {noreply, State}.
handle_info(ok = _Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

fsm_pid(SessionId)->
    case sync_store:lookup(SessionId) of
    {ok, Pid}->
	Pid;
    {error, not_found}->
	{ok, Pid} = sync_server_sup:start_child(),
	sync_store:store(SessionId, Pid),
	Pid
    end.