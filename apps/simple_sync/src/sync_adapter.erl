-module	(sync_adapter).
-include_lib("inets/include/httpd.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([do/1, log/3]).

% SyncML I/F
message(SyncMLRec)->
    sync_agent:message(template(SyncMLRec)).

do(Req) ->
    case check_headers(Req) of
    {ok, CType} ->
	case CType of
	"application/vnd.syncml+xml"->
	    Body = Req#mod.entity_body,
	    error_logger:info_msg("Content-type parsed, syncml+xml", []);
	"application/vnd.syncml+wbxml"->
	    {ok, Body} = wbxml:xml(Req#mod.entity_body),
	    error_logger:info_msg("Encoded xml:~s~n", [Body])
	end,
	Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
	{Acc, P, S};
	(X, Acc, S) ->
	    {[X|Acc], S}
	end,
	{XML, _Rest} = xmerl_scan:string(lists:flatten(io_lib:format("~s",[Body])), [{space, normalize}, {acc_fun, Acc}]),
	ResponseBody = xmerl:export_simple([message(XML)], xmerl_xml),
	error_logger:info_msg("Response body:", [ResponseBody]),
	{proceed, [{response, {response, [{content_type, CType}], ResponseBody}}]};
    {error, false} ->
	error_logger:info_msg("Request contains unappropriate headers!~p~n", [Req#mod.parsed_header]),
	done
    end.

check_headers(Req)->
    % TODO: Request/response headers, methods etc.
    % - General
    % Cache-Control:no-store / private
    % Transfer-Encoding: chunked
    % - Request
    % Accept: mediatype
    % Accept-Charset:utf-8 or status 406
    % User-Agent:.
    case lists:keyfind("content-type", 1, Req#mod.parsed_header) of
    {_Key, CType} ->
	{ok, CType};
    false ->
	{error, false}
    end.

template(E=#xmlElement{name='SyncML'})->
    lists:flatten(xmerl_xs:xslapply(fun template/1, E));
template(E=#xmlElement{name = 'SyncHdr'})->
    [{"init_data", lists:flatten(xmerl_xs:xslapply(fun template/1, E))}];
template(E=#xmlElement{parents=[{'SyncHdr',_}|_], name='SessionID'})->
    Session_ID = xmerl_xs:value_of(xmerl_xs:select(".", E)),
    [{"session_id", lists:nth(1, Session_ID)}];
template(E=#xmlElement{name = 'SyncBody'})->
    lists:flatten(xmerl_xs:xslapply(fun template/1, E));
template(E=#xmlElement{parents=[{'SyncBody',_}|_], name = 'Sync'})->
    [{"sync", lists:flatten(xmerl_xs:xslapply(fun template/1, E))}];
template(E) ->
    xmerl_xs:built_in_rules(fun template/1, E).

log(SessionID, Env, _Input) ->
    mod_esi:deliver(SessionID, ["Content-Type:text/html\r\n\r\n" | log(Env)]).

log([])->
    "<ul><li>?error - for error log</li>
    <li>?transfer - for transfer log</li><ul>";
log([{query_string, "error"} | _Env])->
    {ok, Log} = file:read_file("log/error.log"),
    re:replace(erlang:binary_to_list(Log), "\n", "<br/>", [global, {return, list}]);
log([{query_string, "transfer"} | _Env]) ->
    {ok, Log} = file:read_file("log/transfer.log"),
    re:replace(erlang:binary_to_list(Log), "\n", "<br/>", [global, {return, list}]);
log([{_Key, _Value} | Env]) ->
    log(Env).