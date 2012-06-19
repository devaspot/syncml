-module(sync_engine).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call(_Call, _From, State)->
    Result = "Res",
    {stop, normal, Result, State}.
handle_cast(_Call,State)->
    {noreply, State}.
handle_info(Info, State)->
    error_logger:info_msg("Sync Engine ->handle_info", [Info]),
    {noreply, State}.
terminate(Reason, _State)->
    error_logger:info_msg("terminate", [Reason]),
    ok.
code_change(_OldVsn, State, _Extra)->
    {ok, State}.