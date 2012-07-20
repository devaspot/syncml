-module(sync_adapter).
-include_lib("inets/include/httpd.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([do/1, log/3]).
-import(xmerl_xs, [xslapply/2, value_of/1, select/2, built_in_rules/2]).

% SyncML I/F
message(SyncMLRec)->
    [{_, InitData}|Body] = template(SyncMLRec),
    Response = simple_sync:message(init_engine(InitData), Body),
    {'SyncML', [{'SyncHdr', []}, {'SyncBody', Response}]}.

%% httpd
do(Req) ->
    case check_headers(Req) of
    {ok, CType} ->
	case CType of
	"application/vnd.syncml+xml"->
	    Body = Req#mod.entity_body;
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
	ResponseBody = lists:flatten(xmerl:export_simple([message(XML)], xmerl_xml)),
	error_logger:info_msg("Response body ~p~n:", ResponseBody),
	{proceed, [{response, {response, [{content_type, CType}], ResponseBody}}]};
    {error, false} ->
	error_logger:info_msg("Request contains unappropriate headers!~p~n", [Req#mod.parsed_header]),
	done
    end.

%% Utils
init_engine(InitData)->
    {_, SessionID} = lists:keyfind('SessionID', 1, InitData),
    case sync_store:lookup(SessionID) of
    {ok, Pid} ->
	Pid;
    {error, not_found}->
	{ok, Pid} =sync_server_sup:start_child(InitData),
	sync_store:store(SessionID, Pid),
	Pid
    end.

check_headers(Req)->
    % Cache-Control:no-store / private
    % Transfer-Encoding: chunked
    % Accept: mediatype
    % Accept-Charset:utf-8 or status 406
    % User-Agent:.
    case lists:keyfind("content-type", 1, Req#mod.parsed_header) of
    {_Key, CType} ->
	{ok, CType};
    false ->
	{error, false}
    end.

%% Parser
template(E=#xmlElement{name='SyncML'})->
    lists:flatten(xslapply(fun template/1, E));
template(E=#xmlElement{name = 'SyncHdr'})->
    {init_data, xslapply(fun template/1, E)};
template(E=#xmlElement{parents=[{'SyncHdr',_}|_], name='SessionID'})->
    [SessionID|[]] = value_of(select(".", E)),
    {'SessionID', SessionID};
template(E=#xmlElement{name='CmdID'})->
    [CmdID|[]] = value_of(select(".", E)),
    {'CmdID', CmdID};
template(E=#xmlElement{name='Target'}) ->
    [LocURI|[]] = value_of(select("LocURI", E)),
    {'Target', LocURI};
template(E=#xmlElement{name='Source'})->
    [LocURI|[]] = value_of(select("LocURI", E)),
    {'Source', LocURI};
template(E=#xmlElement{name = 'SyncBody'})->
    xslapply(fun template/1, E);
template(E=#xmlElement{parents=[{'SyncBody',_}|_], name = 'Sync'})->
    {'Sync', xslapply(fun template/1, E)};
template(E=#xmlElement{parents=[{'Sync',_}|_], name='Add'})->
    {'Add', xslapply(fun template/1, E)};
template(E=#xmlElement{name='Item'})->
    {'Item', xslapply(fun template/1, E)};
template(E=#xmlElement{parents=[{'Item',_}|_], name='Data'})->
    [Data|[]] = value_of(select(".", E)),
    {'Data', Data};
template(_E=#xmlElement{name='Final'})->
    {'Final', ok};
template(_E=#xmlElement{name=Name})->
    {Name, []};
template(E) ->
    built_in_rules(fun template/1, E).

%% Log
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