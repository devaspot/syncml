-module(simple_sync).
-export([test/3, service/3]).
-include_lib("xmerl/include/xmerl.hrl").

test(SessionID, Env, _Input) ->
    mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n" | format(Env)]).

format([]) ->
    "";
format([{Key, Value} | Env]) ->
    [io_lib:format("<b>~p:</b> ~p<br />\~n", [Key, Value]) | format(Env)].

service(SessionID, _Env, _Input) ->
     mod_esi:deliver(SessionID, ["Content-Type: text/html\r\n\r\n", "Service."]).


