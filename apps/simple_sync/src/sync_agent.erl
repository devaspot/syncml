-module(sync_agent).
-behaviour(gen_server).
-export([start_link/1, package/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2, code_change/3]).
-record(state, {fsm_pid, package}).

package(Package)->
    {ok, Pid} = sync_agent_sup:start_child(Package),
    gen_server:call(Pid, package).

start_link(Package)->
    gen_server:start_link(?MODULE, [Package], []).

init([Package])->
    {ok, Pid} = sync_engine:start_link(),
    {ok, #state{fsm_pid = Pid, package=Package}}.

handle_cast(_Call, State)->
    {noreply, State}.

handle_call(package, _From, State )->
    Result = sync_engine:package(State#state.fsm_pid, State#state.package),
    {reply, Result, State};
handle_call(_Call, _From, State) ->
    {reply, {ok, State}}.

handle_info(ok = _Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.