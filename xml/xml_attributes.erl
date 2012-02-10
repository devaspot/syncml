-module(xml_attributes).
-copyright('Copyright (c) 1997 Ericsson Telecommunications AB').

%% XML attribute parser and generator version 1.0
%% Author Joe Armstrong <joe@cslab.ericsson.se>
%% The XML grammar sucks
%% Basically it is like this

%% <!ATTLIST termdef
%%           id      ID      #REQUIRED
%%           name    CDATA   #IMPLIED>
%% => {termdef, [{"id",{id,required}},{name,{"cdata",implied}}]}

%% <!ATTLIST list
%%           type    (bullets|ordered|glossary)  "ordered">
%% => {list, [{"type", 
%%               {enum,["bullets","ordered","glossary"],
%%                     "ordered"}}]}.

%% <!ATTLIST form
%%           method  CDATA   #FIXED "POST">
%% => {form, [{"method",{fixed,"POST"}}]}

%% ATTLIST Name Key ID|ID|IDREF  #REQUIRED|#IMPLIED|#FIXED|Literal
%%             Key (a|b|c)      

-export([parse_definition/1, verify/3, check_ids/0]).

-import(xml_tokenise, [tupStr/1]).
-import(xml, [fatal/1, fatal/2]).
-import(lists, [member/2, reverse/1, sort/1,reverse/2]).

parse_definition(L) ->
    %% io:format("parse_attlist:~p~n", [L]),
    case (catch p_attlist(L)) of
	{'EXIT', W} ->
	    {error, W};
	{error, W} ->
	    {error, W};
	Result ->
	    {ok, Result}
    end.

p_attlist([Name|T]) ->
    Vals = p_attlist_elements(T),
    {tupStr(Name), Vals}.

p_attlist_elements([Key,"ID"|T1]) ->
    {Def, T2} = p_default(T1),
    [{Key,{id,Def}}|p_attlist_elements(T2)];
p_attlist_elements([Key,"CDATA"|T1]) ->
    {Def, T2} = p_default(T1),
    [{Key,{cdata,Def}}|p_attlist_elements(T2)];
p_attlist_elements([Key,"IDREF"|T1]) ->
    {Def, T2} = p_default(T1),
    [{Key,{idref,Def}}|p_attlist_elements(T2)];
p_attlist_elements([Key,'('|T1]) ->
    {Enum, T2} = p_list(T1),
    {Def, T3} = p_default(T2),
    [{Key,{enum,Enum,Def}}|p_attlist_elements(T3)];
p_attlist_elements([]) ->
    [];
p_attlist_elements(Other) ->
    throw({error,["Cannot parse ATTLIST elements", Other]}).

p_default(['#', "REQUIRED"|T])           -> {required,T};
p_default(['#', "IMPLIED"|T])            -> {implied,T};
p_default(['#', "FIXED", {lit,_,Str}|T]) -> {{fixed,Str},T};
p_default([{lit,_,Str}|T])               -> {{default,Str}, T};
p_default(T) ->
    throw({error, ["expecting #REQUIRED, #IMPLIED, #FIXED or a string found", T]}).

p_list([Str,')'|T]) -> {[Str], T};
p_list([Str,'|'|T]) ->
    {R,T1} = p_list(T),
    {[Str|R], T1}.

%%% verify

verify(Line, Defined, Args) ->
    Parse = parse_supplied_value(Line, Args),
    check_attrs(Line, Defined, Parse, []).

%% +type check_attrs(int(), [def()], [{string(),string()}] ->
%%   [{string(),string()}]

check_attrs(Line, [{Name, Type}|T], Supplied, L) ->
    {Result, Supplied1} = find(Name, Supplied),
    %% io:format("check:~p => ~p~n", [Type, Result]),
    case check(Type, Result) of
	{ok, Val} ->
	    check_attrs(Line, T, Supplied1, [{Name, Val}|L]);
	{error, X} ->
	    fatal(Line, ["bad attribute"] ++ X),
	    check_attrs(Line, T, Supplied1, L)
    end;
check_attrs(Line, [], [], L) ->
    reverse(L);
check_attrs(Line, [], More, L) ->
    fatal(Line, [" some attributes were not processed ",More]),
    [].

check({enum,Vals,Default}, {found, V}) ->
    case member(V, Vals) of
	true -> {ok, V};
	false -> {error, ["incorrect attribute found ", V,
			  " expecting one of" ,Vals]}
    end;
check({enum,Vals,Default}, not_found) ->
    {ok, Default};
check({Type, {fixed,Str}}, {found, Str}) ->
    assert_id(Type, Str),
    {ok, Str};
check({Type, {fixed,Str}}, {found, Str1}) ->
    {error, ["expecting the #FIXED string ",Str," found ",Str1]};
check({Type, {fixed,Str}}, not_found) ->
    {error, ["expecting the #FIXED string ",Str," but no value supplied"]};
check({Type, required}, {found, Str}) ->
    assert_id(Type, Str),
    {ok, Str};
check({Type, required}, not_found) ->
    {error, [Type,"is required but no value supplied"]};
check({Type, implied}, {found, Str}) ->
    assert_id(Type, Str),
    {ok, Str};
check({Type, implied}, not_found) ->
    assert_id(Type, ""),
    {ok, ""};
check(Required, Supplied) ->
    exit({in,check,Required,Supplied}).

assert_id(id, Str) -> put(ids, [Str|get(ids)]);
assert_id(idref, Str) -> put(idrefs, [Str|get(idrefs)]);
assert_id(_, _) -> true.

find(Key, Dict) -> find(Key, Dict, []).

find(Key, [{Key,Val}|Dict], L) ->
    {{found, Val}, reverse(L, Dict)};
find(Key, [H|Dict], L) ->
    find(Key, Dict, [H|L]);
find(Key, [], L) ->
    {not_found, L}.

parse_supplied_value(Line, [Name,'=',{lit,_,Str}|T]) ->
    [{Name,Str}|parse_supplied_value(Line,T)];
parse_supplied_value(_,[]) -> [];
parse_supplied_value(Line,Other) ->
    fatal(Line, ["bad attribute",Other]),
    [].

%% check_ids() -> ok | error

check_ids() ->
    %% check if the ids are unique
    Ids = sort(get(ids)),
    case duplicates(Ids, []) of
	[] -> 
	    Refs = sort(get(idrefs)),
	    case duplicates(Refs, []) of
		[] -> 
		    C = [X||X<-Ids,not member(X,Refs)],
		    case C of 
			[] ->
			    true ;
			_ ->
			    fatal(["there are some ID conflicts, the following IDs were defined but have no corresponding IDREFs", C])
		    end;
		L1 ->  
		    fatal(["there are some duplicated IDREFs ", L1])
	    end;
	L ->  fatal(["There are some duplicated Ids ", L])
    end.

duplicates([X,X|T], L) -> duplicates(T, [X|L]);
duplicates([_|T], L) -> duplicates(T, L);
duplicates([], L) -> [].





