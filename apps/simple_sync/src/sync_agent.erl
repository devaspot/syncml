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
    {ok, #state{fsm_pid = fsm_pid(get("session_id")), msg="Msg"}}.

handle_call(msg, _From, State )->
    Result = sync_engine:message(State#state.fsm_pid, State#state.msg),
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
	{ok, Pid} = sync_engine_sup:start_child(),
	sync_store:store(SessionId, Pid),
	Pid
    end.

template(E=#xmlElement{parents=[{'SyncHdr',_}|_], name='SessionID'})->
    put("session_id", xmerl_xs:value_of(xmerl_xs:select(".", E)));
template(E)->
    xmerl_xs:built_in_rules(fun template/1, E).