-module(syncml_11).
-author('maxim@synrc.com').
-export([create_db/0,lookup_tag/2]).

-define(SPACE,32).

-include("../wbxml_bytecodes.hrl").
-include("syncml_11_bytecodes.hrl").
-include("../xmerl.hrl").

lookup_tag(Code,TagPage) ->
    case lists:keysearch(Code,1,lists:nth(TagPage,?SYNCML_TAG_TOKENS)) of
    {value,{Code,Val}} ->
        Val;
    _ ->
        io:format("UNKNOWN TAG: ~p~n",[Code]),
        throw({error,unknown_tag_code})
    end.

create_db() ->
    WmlTbl=ets:new(wmldb,[set, private]),
    init_tag_tokens(0,lists:nth(0,?SYNCML_TAG_TOKENS),WmlTbl),
    init_tag_tokens(1,lists:nth(1,?SYNCML_TAG_TOKENS),WmlTbl).

init_tag_tokens(_,[],_) ->
    ok;
init_tag_tokens(Page,[{Code,Val}|TokenList],WmlTbl) ->
    ets:insert(WmlTbl,{Page*256+Code,Val}),
    ets:insert(WmlTbl,{Val,Code+256*Page}),
    init_tag_tokens(Page,TokenList,WmlTbl).
