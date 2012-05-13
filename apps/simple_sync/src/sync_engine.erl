-module(sync_engine).
-behaviour(gen_fsm).
-export([start_link/0, package/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([build_packet/3]).
-record(state, {}).

start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

package(FSMPid, Package)->
    gen_fsm:sync_send_event(FSMPid, {package, Package}).

init([]) ->
    {ok, build_packet, #state{}}.

build_packet({package, Package}, _From, State)->
    Reply = Package, %"process_package(Package)"
    {stop, normal, Reply, State}.

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