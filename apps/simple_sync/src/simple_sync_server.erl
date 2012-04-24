-module(simple_sync_server).
-behaviour(gen_server).
-include_lib("inets/include/httpd.hrl").
-export([start_link/0, do/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

do(Req) ->
    case Req#mod.method of
    "POST" ->
    	io:format("<p>POST: ~p</p>",[Req#mod.parsed_header]);
    "GET" ->
    	io:format("<p>GET: ~p</p>", [Req#mod.parsed_header])
    end,
    done.

init([]) ->
    {ok, #state{}}.

handle_call(_Call, _From, State) ->
    {reply, {ok, State}}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(ok = _Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
