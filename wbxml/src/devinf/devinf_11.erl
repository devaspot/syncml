-module(devinf_11).
-author('maxim@synrc.com').
-export([create_db/0,lookup_tag/1]).

-define(SPACE,32).

-include("../wbxml_bytecodes.hrl").
-include("devinf_11_bytecodes.hrl").
-include("../xmerl.hrl").

lookup_tag(Code) ->
    case lists:keysearch(Code,1,?DEVINF_TAG_TOKENS) of
    {value,{Code,Val}} ->
        Val;
    _ ->
        throw({error,unknown_tag_code})
    end.

create_db() ->
    WmlTbl=ets:new(wmldb,[set, private]),
    init_db2(?DEVINF_TAG_TOKENS,WmlTbl).

init_db2([],_) ->
    ok;
init_db2([{Code,Val}|TokenList],WmlTbl) ->
    ets:insert(WmlTbl,{Code,Val}),
    ets:insert(WmlTbl,{Val,Code}),
    init_db2(TokenList,WmlTbl).


