-module(simple_sync).
-export([message/1]).

message(Msg)->
    %error_logger:info_msg("Message passed to symple_sync API", [Msg]),
    sync_agent:message(Msg).