-module(simple_sync_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Srv = {simple_sync_server, {simple_sync_server, start_link, []}, permanent, 2000, worker, [simple_sync_server]},
    {ok, { {one_for_one, 0, 1}, [Srv]} }.

