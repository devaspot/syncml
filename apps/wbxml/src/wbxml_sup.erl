-module(wbxml_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/0, init/1]).

start_link()->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child()->
    supervisor:start_child(?MODULE, []).

init([])->
    Encoder = {wbxml_encoder, {wbxml_encoder, start_link, []},
    temporary, brutal_kill, worker, [wbxml_encoder]},

    {ok, {{simple_one_for_one, 0, 1}, [Encoder]}}.