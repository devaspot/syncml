-module(simple_sync).
-export([message/1]).

message(Msg)->
    sync_agent:message(Msg).