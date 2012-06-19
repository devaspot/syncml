-module(sync_agent).
-behaviour(gen_server).
-export([start_link/1, message/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-record(state, {fsm_pid, msg}).
-include_lib("xmerl/include/xmerl.hrl").

message(Msg)->
    {ok, Pid} = sync_agent_sup:start_child(Msg),
    gen_server:call(Pid, msg).

start_link(Msg)->
    gen_server:start_link(?MODULE, [Msg], []).

init([Msg])->
    template(Msg),
    State = #state{fsm_pid = fsm_pid(get("session_id"))},
    {ok, State}.

handle_call(msg, _From, State )->
    % Verify message: Final? if no ask for more with Alert 222.
    % No Alert if Final
    Result = simple_sync:message(State#state.fsm_pid, State#state.msg),
    error_logger:info_msg("engine call result:", [Result]),
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

% Alert | Atomic | Copy | Exec | Get | Map | Put | Results | Search |Sequence | Status | Sync)+, Final?
%template(_E=#xmlElement{parents=[{'SyncBody',_}|_], name='Alert'})->
%    error_loger:info_msg("Alert!");
template(E=#xmlElement{parents=[{'SyncHdr',_}|_], name='SessionID'})->
    put("session_id", xmerl_xs:value_of(xmerl_xs:select(".", E)));
%template(_E=#xmlElement{parents=[{'SyncBody',_}|_], name='Final'})->
%    error_logger:info_msg("Final");
%template(E=#xmlElement{parents=[{'SyncBody',_}|_], name='Sync'})->
%    xmerl_xs:xslapply(fun template/1, E);
%template(E=#xmlElement{parents=[{'SyncBody',_}|_], name='Put'})->
    gen_fsm:send_event(self(), {put_cmd, E});
%template(E=#xmlElement{parents=[{'Sync',_}|_], name='Add'})->
%    gen_fsm:send_event(self(), {add_cmd, E});
%template(E=#xmlElement{parents=[{'Sync',_}|_], name='Replace'})->
%    gen_fsm:send_event(self(), {replace_cmd, E});
%template(E=#xmlElement{parents=[{'SyncBody',_}|_], name='Final'})->
%    gen_fsm:send_event(self(), {final, E});
template(E)->
    xmerl_xs:built_in_rules(fun template/1, E).