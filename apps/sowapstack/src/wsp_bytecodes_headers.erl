%%% File    : wsp_bytecodes_headers.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP HTTP and WAP header encoding
%%% Created : 14 Oct 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_bytecodes_headers).
-author('johblo@dragon.cellpt.se').
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_bytecodes_headers.erl,v 1.1.1.1 2001/07/04 14:51:11 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:11 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").
-author('johblo@dragon.cellpt.se').

-export([encode_headers/2,decode_headers/1,decode_header_field/1,
	 encode_content_type/2,decode_content_type/1,
	 encode_language1/1,encode_language2/2,encode_language3/1,
	 decode_language/1,decode_wellknown_language/1,
	 encode_charset1/1,encode_charset2/2,encode_charset3/1,
	 decode_charset/1,decode_wellknown_charset/1,
	 encode_challenge/2,decode_challenge/1,
	 encode_credentials/2,decode_credentials/1,
	 encode_field/3,decode_field/2,
	 decode_integer/1,decode_short_integer/1,decode_long_integer/1,
	 encode_integer/1,encode_short_integer/1,encode_long_integer/1,
	 encode_text_string/1,decode_text_string/1,
	 encode_uri/1,decode_uri/1,
	 encode_token_text/1,decode_token_text/1,
	 encode_extension_media/1,decode_extension_media/1,
	 encode_parameters/2,decode_parameter_field/1,
	 encode_parameter/3,decode_parameter/1,
	 encode_untyped_val/1,decode_untyped_val/1,
	 decode_parlist/1,encode_qval_list/1,decode_qval/1,
	 encode_valuelength/1,decode_valuelength/1,
	 pp_headers/1]).


-include("wsp.hrl").

-import(wap_common,[pack_uintvar/1,unpack_uintvar/1,
		    pack_uint8/1,unpack_uint8/1]).


%% =============================================================================
%% Encode all headers
%% encode_headers([{HeaderField,HeaderValue},...],[]) -> Data
%% encode_headers([{Page,HeaderField,HeaderValue},...],[]) -> Data
%%    HeaderField -> atom()
%%    HeaderValue -> Parsed value depends on HeaderField
%%    Page -> int() or atom()
%%    Data -> list()
%% 
%% Note:
%% - Header encodings that requires the Default header code page or has no known
%%   header encoding are given with {Key,Val} tuples, all other are given with
%%   {Page,Key,Val} tuples
%% - The header list is not sorted with respect to header code pages, thus it is
%%   recommended that this is made by the application, to avoid unnecessary
%%   header code page switching.
%% - Other header encode pages are supposed to be stored in modules with names
%%   wsp_headers_pageX where X is the page number. This module should export
%%   the two functions encode_header/3 and decode_headers/1
%% - Always encode as text if the parsed header doesn't have a rule for encoding
%%   or the encoding grammar does not match parsed data.
encode_headers(Headerlist,Env) ->
    EncodeVers=lookup_encodevers(?DEFAULT_PAGE,Env#env.encoding_version),
    encode_headers(Headerlist,{?DEFAULT_PAGE,EncodeVers},Env,[]).

encode_headers([],_,_,Output) ->
    Output;
encode_headers([{Key,Val} | T],{Page,EncVers},Env,Output) ->
    case catch wsp_headers_page1:encode_header(Key,Val,EncVers) of
	{Type,Reason} when Type=='EXIT';Type==error ->
	    ?warning("Couldn't encode_header ~p:~p got ~p",
		   [Key,Val,Reason],encode_headers),
	    encode_headers(T, {Page,EncVers},Env,
			   Output ++ encode_text_string(Key) ++
			   encode_text_string(flatten(Val)));
	EncHeader ->
	    encode_headers(T,{Page,EncVers},Env,Output ++ EncHeader)
    end;
encode_headers([{Page,Key,Val}|T],{Page,EncVers},Env,Output) ->
    Module=list_to_atom("wsp_headers_page"++integer_to_list(Page)),
    case catch apply(Module,encode_header,[Key,Val,EncVers]) of
	{Type,Reason} when Type=='EXIT';Type==error ->
	    ?debug("Couldn't encode_header ~p ~p",
		   [{Page,Key,Val},Reason],encode_headers),
	    encode_headers(T, {Page,EncVers},Env,
			   Output ++ encode_text_string(Key) ++
			   encode_text_string(flatten(Val)));
	EncHeader ->
	    encode_headers(T,{Page,EncVers},Env,Output ++ EncHeader)
    end;
encode_headers([{OtherPage,Key,Val}|T],{Page,EncVers},Env,Output) ->
    case lookup_hcp(OtherPage,(Env#env.cap)#cap.header_code_pages) of
	Code when is_integer(Code) ->
	    EncPage=encode_headercodepage(Code),
	    Module=list_to_atom("wsp_headers_page"++integer_to_list(Code)),
	    case catch apply(Module,encode_header,[Key,Val,EncVers]) of
		{Type,Reason} when Type=='EXIT';Type==error ->
		    ?warning("Couldn't encode_header ~p ~p",
			     [{OtherPage,Key,Val},Reason],encode_headers),
		    encode_headers(T, {Page,EncVers},Env,
				   Output ++ encode_text_string(Key) ++
				   encode_text_string(flatten(Val)));
		EncHeader ->
		    apply(Module,encode_header,[Key,Val,EncVers]),
		    OtherEncodeVers=
			lookup_encodevers(OtherPage,Env#env.encoding_version),
		    encode_headers(T,{OtherPage,OtherEncodeVers},Env,
				   Output ++ EncPage ++ EncHeader)
	    end;
	_ ->
	    encode_headers(T, {Page,EncVers},Env,
			   Output ++ encode_text_string(Key) ++
			   encode_text_string(Val))
    end.

%% .............................................................................
lookup_encodevers(Page,EVlist) ->
    case lists:keysearch(Page,1,EVlist) of
        {value,{Page,M}} ->
            M;
        _ ->
            case Page of
		?DEFAULT_PAGE -> 2;
		_ ->  undefined
	    end
    end.

%% .............................................................................
%% Check if the Headercode page references is supported and thus negotiated.
lookup_hcp(PageName,HCPList) ->
    case lists:keysearch(PageName,1,HCPList) of
        {value,{_,Code}} ->
            Code;
        _ ->
	    false
    end.

%% -----------------------------------------------------------------------------
%% Decode all headers
%% decode_headers(Data) -> [tuple(),...]
%%    Data -> list() that only contains the headerlist
decode_headers(<<>>)->
    [];
decode_headers(BinHeaders) ->
    Data1=decode_headers2(binary_to_list(BinHeaders),?DEFAULT_PAGE),
    Data2=join_http_headers(Data1),
    fix_range_header(Data2).
    
decode_headers2([],_) ->
    [];
decode_headers2(C,?DEFAULT_PAGE) ->
    case decode_header_field(C) of
	{newpage,Page,Input} ->
	    ?trace("Swapping Header Code Page to ~p",[Page],decode_headers2),
	    decode_headers2(Input,Page);
	{code,Input,Key} ->
	    case catch wsp_headers_page1:decode_header_value(Key,Input) of
		{error,Reason} ->
		    {ok,Input1}=skip_header_val(Input),
		    ?error("Skipped header value ~p now got ~p",
			   [Reason,Input1],decode_headers2),
		    decode_headers2(Input1,?DEFAULT_PAGE);
		{Input1,Value} ->
		    FValue=print_header_val(Key,Value),
		    [{Key,FValue}] ++ decode_headers2(Input1,?DEFAULT_PAGE)
	    end;
	{Input,Key} ->
	    {Input1,Value}=decode_token_text(Input),
	    [{Key,Value}] ++ decode_headers2(Input1,?DEFAULT_PAGE)
    end;
decode_headers2([H|C],Page) ->
    if
	0=<H,H=<31 ->
	    ?debug("Swapping Header Code Page ~p",[H],decode_headers2),
	    decode_headers2(C,H);
	127==H ->
	    ?debug("Swapping Header Code Page ~p",[hd(C)],decode_headers2),
	    decode_headers2(tl(C),hd(C));
	32=<H,H<127 ->
	    {Input,Key}=wsp_bytecodes:decode_string([H|C]),
	    case skip_header_val(Input) of
		{error,_Reason} ->
		    ?error("Illformed header ~p",[Key],decode_headers2),
		    decode_headers2([],Page);
		{ok,A} ->
		    decode_headers2(A,Page)
	    end;
	128=<H ->
	    {Input,Key}=decode_short_integer([H|C]),
	    case skip_header_val(Input) of
		{error,_Reason} ->
		    ?error("Illformed header ~p",[Key],decode_headers2),
		    decode_headers2([],Page);
		{ok,A} ->
		    decode_headers2(A,Page)
	    end
    end.


pp_headers(Input) ->
    case catch pp_headers2(binary_to_list(Input),?DEFAULT_PAGE) of
	{'EXIT',Reason} ->
	    io:format("ERROR pp_headers: ~p",[Reason]);
	{error,Reason} ->
	    io:format("ERROR pp_headers: ~p",[Reason]);
	Error ->
	    Error
    end.

pp_headers2([],_) ->
    ok;
pp_headers2(C,?DEFAULT_PAGE) ->
    case decode_header_field(C) of
	{newpage,Page,Input} ->
	    io:format("~nSwapping Header Code Page to ~w ~n",[Page]),
	    pp_headers2(Input,Page);
	{error,Reason} ->
	    io:format("~nError: ~w when trying to decode fieldname~n Code ~w~n",
		      [Reason,C]);
	{code,C1,Key} ->
	    io:format("  Header ~p:",[Key]),
	    case wsp_headers_page1:decode_header_value(Key,C1) of
		{error,Reason} ->
		    {ok,C2}=skip_header_val(C1),
		    io:format("~nError: ~w when trying to decode field value~n Didn't expect ~w~n",
			      [Reason,C1--C2]);
		{C2,Value} ->
		    Hlist=lists:sublist(C,length(C)-length(C2)),
		    io:format("~p from ~w~n",[Value,Hlist]),
		    pp_headers2(C2,?DEFAULT_PAGE)
	    end;
	{C1,Key} ->
	    {C2,Value}=decode_token_text(C1),
	    Hlist=lists:sublist(C,length(C)-length(C2)),
	    io:format("  Header ~p:",[Key]),
	    io:format("~p from ~w~n",[Value,Hlist]),
	    pp_headers2(C2,?DEFAULT_PAGE)
    end;
pp_headers2([H|C],_Page) when 1=<H,H=<31 ->
    io:format("~nSwapping Header Code Page to ~w~n",[H]),
    pp_headers2(C,H);
pp_headers2([H|C],Page) when 32=<H,H<127 ->
    {C1,F}=wsp_bytecodes:decode_string([H|C]),
    io:format("Skipping header (unknown encoding) ~p",[F]),
    {ok,Input1}=skip_header_val(C1),
    io:format(" Code:~w~n",
	      [lists:sublist([H|C],1,length([H|C])-length(Input1))]),
    pp_headers2(Input1,Page);
pp_headers2([127|C],_Page) ->
    io:format("~nSwapping Header Code Page to ~w~n",[hd(C)]),
    pp_headers2(tl(C),hd(C));
pp_headers2([H|C],Page) when 128=<H ->
    {C1,F}=decode_short_integer([H|C]),
    io:format("Skipping header (unknown encoding) ~w",[F]),
    Input1=
	case skip_header_val(C1) of
	    {error,Reason} ->
		io:format("Headers seems to be illformed ~w ~n",
			  [Reason]),
		[];
	    {ok,A} ->
		A
	end,
    io:format(" Code:~w~n",
	      [lists:sublist([H|C],1,length([H|C])-length(Input1))]),
    pp_headers2(Input1,Page).


%%% ............................................................................
%% The header was encoded in an invalid or unknown way, so skip this header.
%% Note that this is only possible if the header encoding rules are used in the
%% proper way, otherwise the decoding fails and we shall return an Abort of the
%% transaction.
skip_header_val([]) ->
    {error,invalid_headers};
skip_header_val([H|Input]) when 0=<H,H=<31 ->
    {C1,Len}=decode_valuelength([H|Input]),
    {ok,lists:nthtail(Len,C1)};
skip_header_val([H|Input]) when 32=<H,H=<127 ->
    {Input1,_}=decode_text_string([H|Input]),
    {ok,Input1};
skip_header_val([H|Input]) when 128=<H ->
    {ok,Input}.


join_http_headers([]) ->
    [];
join_http_headers([{K1,V1},{K1,V2}|Hlist]) ->
    join_http_headers([{K1,V1++","++V2}|Hlist]);
join_http_headers([H|Hlist]) ->
    [H]++join_http_headers(Hlist).

fix_range_header([]) ->
    [];
fix_range_header([{'range',V}|Rest]) ->
    [{'range',"bytes="++V}]++fix_range_header(Rest);
fix_range_header([H|Rest]) ->
    [H]++fix_range_header(Rest).


%% -----------------------------------------------------------------------------
%% Well-known Header Field Name Assignments
%% Headers in WSP_WELL_KNOWN_HEADERS are stored as tuples
%% {Encoding,EncodingVersion,HeaderFieldName}
%% Note: It is assumed that Headers encoded with other Header Code Pages than
%% the WAP specific ones (1-16) are handled elsewhere.
encode_header_field(Val,EncVers) ->
    case encode_field(Val,EncVers,?WSP_WELL_KNOWN_PAGE1_HEADERS) of
	{ok,undef_field} ->
	    encode_text_string(atom_to_list(Val));
	Code ->
	    [Code]
    end.

decode_header_field([Page| Rest]) when 1=<Page,Page=<31 ->
    {newpage,Page,Rest};
decode_header_field([127,Page| Rest]) ->
    {newpage,Page,Rest};
decode_header_field([Code| Rest]) when Code>127 ->
    {code,Rest,wsp_headers_page1:decode_header_field(Code)};
decode_header_field(C) ->
    {C1,Str}=decode_text_string(C),
    {C1,list_to_atom(Str)}.

%% -----------------------------------------------------------------------------
%% Well-known Content Type Assignments
%% Content Types in WSP_WELL_KNOWN_CONTENT_TYPES are stored as tuples
%% {Encoding,EncodingVersion,ContentTypeName}
encode_content_type(Val,EncVers) ->
    case encode_field(Val,EncVers,?WSP_WELL_KNOWN_CONTENT_TYPES) of
	{ok,undef_field} ->
	    encode_text_string(atom_to_list(Val));
	Code ->
	    [Code]
    end.

decode_content_type(Code) ->
    case decode_field(Code,?WSP_WELL_KNOWN_CONTENT_TYPES) of
	{ok,undef_code} ->
	    {error,unknown_content_type};
	Val ->
	    {ok,Val}
    end.



%% -----------------------------------------------------------------------------
%% Well-known Language Assignments
%% Languages in WSP_WELL_KNOWN_LANGUAGES are stored as tuples
%% {Encoding,[Name1,...]}
encode_language1(Lang) ->
    case encode_field2(Lang,?WSP_WELL_KNOWN_LANGUAGES) of
	{ok,undef_field} ->
	    encode_token_text(Lang);
	Code ->
	    encode_integer(Code)
    end.

encode_language2(Lang,Eqv) ->
    case encode_field2(Lang,?WSP_WELL_KNOWN_LANGUAGES) of
	{ok,undef_field} ->
	    encode_token_text(Lang);
	Code ->
	    Out=encode_integer(Code)++Eqv,encode_valuelength(Out)++Out
    end.

encode_language3(Lang) ->
    case encode_field2(Lang,?WSP_WELL_KNOWN_LANGUAGES) of
	{ok,undef_field} ->
	    encode_token_text(Lang);
	Code when Code > 127 ->
	    Eit=encode_integer(Code),encode_valuelength(Eit)++Eit;
	Code ->
	    encode_short_integer(Code)
    end.


decode_wellknown_language([?ANY_LANGUAGE|Str]) ->
    {Str,'*'};
decode_wellknown_language(Str) ->
    {Str1,Int}=decode_integer(Str),
    {ok,Lang}=decode_language(Int),
    {Str1,Lang}.


decode_language(Code) ->
    case decode_field2(Code,?WSP_WELL_KNOWN_LANGUAGES) of
	{ok,undef_code} ->
	    {error,unknown_language};
	Val ->
	    {ok,atom_to_list(Val)}
    end.

% decode_language(C) ->
%     case hd(C) of
% 	X1 when X1>=32,X1=<127 ->
% 	    decode_text_string(C);
% 	128 ->
% 	    {tl(C),"*"};
% 	X1 ->
% 	    {C1,Val}=decode_integer(C),
% 	    case wsp_bytecodes:decode_language(Val) of
% 		{ok,{undef_code,X}} ->
% 		    throw({error,unknown_language});
% 		Atom ->
% 		    {C1,atom_to_list(Atom)}	    
% 	    end
%     end.


%% .............................................................................
%% Well-known Character sets Assignments
%% Character sets in WSP_WELL_CHARACTER_SETS are stored as tuples
%% {Encoding,[Name1,...]}
encode_charset1(Charset) ->
    case ucs:getMIB(Charset) of
	{error,undefined_charset} ->
	    encode_token_text(Charset);
	Code ->
	    encode_integer(Code)
    end.

encode_charset2(Charset,Eqv) ->
    case ucs:getMIB(Charset) of
	{error,undefined_charset} ->
	    encode_token_text(Charset);
	Code ->
	    Out=encode_integer(Code)++Eqv,encode_valuelength(Out)++Out
    end.

encode_charset3(Charset) ->
    case ucs:getMIB(Charset) of
	{error,undefined_charset} ->
	    encode_token_text(Charset);
	Code when Code > 127 ->
	    Eit=encode_integer(Code),encode_valuelength(Eit)++Eit;
	Code ->
	    encode_short_integer(Code)
    end.

decode_wellknown_charset([128|Str]) ->
    {Str,'*'};
decode_wellknown_charset(Str) ->
    {Str1,Int}=decode_integer(Str),
    {ok,Charset}=decode_charset(Int),
    {Str1,Charset}.

decode_charset(MIB) ->
    case ucs:getCharset(MIB) of
	{error,undefined_mibnum} ->
	    {error,unknown_charset};
	Val ->
	    {ok,atom_to_list(Val)}
    end.
    


%% =============================================================================
encode_field(Val,EV,[{Code,EV2,Val}|_Rest]) when EV>=EV2 -> Code;
encode_field(Val,EV,[_|Rest]) -> encode_field(Val,EV,Rest);
encode_field(_,_,[]) -> {ok,undef_field}.

decode_field(Code,[{Code,_,Val}|_Rest]) -> Val;
decode_field(Code,[_|Rest]) -> decode_field(Code,Rest);
decode_field(_,[]) -> {ok,undef_code}.

%% .............................................................................
encode_field2(Val,[{Code,Vallist}|Rest]) ->
    case lists:member(Val,Vallist) of
	true ->
	    Code;
	_ ->
	    encode_field2(Val,Rest)
    end;
encode_field2(_,[]) ->
    {ok,undef_field}.

decode_field2(Code,[{Code,[Val|_]}|_Rest]) ->
    Val;
decode_field2(Code,[_|Rest]) ->
    decode_field2(Code,Rest);
decode_field2(_,[]) ->
    {ok,undef_code}.

%% -----------------------------------------------------------------------------
%% From 8.4.2.1 WSP1.0, Basic encodings

%% Add a quote character (127) if first character is between 128-255
encode_text_string(Val) when is_atom(Val) ->
    V=atom_to_list(Val),
    case hd(V) of
	H when 127<H,H<256 ->
	    [127] ++ V ++ [0];
	_  -> V ++ [0]
    end;
encode_text_string([H|Val]) when 127<H,H<256 ->
    [127] ++ [H|Val] ++ [0];
encode_text_string(Val) when is_list(Val) ->
    Val ++ [0];
encode_text_string(_Val) ->
    throw({error,invalid_text_string}).

decode_text_string([0|Content]) ->
    {[],Content};
decode_text_string(Content) ->
    Str=string:sub_word(Content,1,0),
    case hd(Str) of
	127 ->
	    {lists:nthtail(length(Str)+1,Content),tl(Str)};
	_ ->
	    {lists:nthtail(length(Str)+1,Content),Str}
    end.
    
%% .............................................................................
encode_token_text(Val) when is_atom(Val) -> atom_to_list(Val) ++ [0];
encode_token_text(Val) -> Val ++ [0].

decode_token_text(Content) ->
    Str=string:sub_word(Content,1,0),
    {lists:nthtail(length(Str)+1,Content),Str}.

%% .............................................................................
%% A quoted string, is text with a pair '"','"' surrounding the text.
%% Add a quote character (34)
encode_quoted_string(Val) ->
    [34] ++ lists:reverse(tl(lists:reverse(tl(Val)))) ++ [0].

decode_quoted_string(Content) ->
    {C1,Str}=decode_text_string(Content),
    {C1,"\""++Str++"\""}.

%% .............................................................................
encode_extension_media(Val) when is_atom(Val) -> atom_to_list(Val) ++ [0];
encode_extension_media(Val) -> Val ++ [0].

decode_extension_media(Content) ->
    Str=string:sub_word(Content,1,0),
    {lists:nthtail(length(Str)+1,Content),Str}.


%% .............................................................................
encode_short_integer(Int) when 0=<Int,Int<128 ->
    [Int+128];
%% The MSB might be added already
encode_short_integer(Int) when 127<Int,Int<256 ->
    [Int];
encode_short_integer(Int) when is_atom(Int) ->
    encode_short_integer(atom_to_integer(Int));
encode_short_integer(_) ->
    throw({error,not_short_integer}).

decode_short_integer([Int|Rest]) when 127<Int,Int<256 ->
    {Rest,Int-128};
decode_short_integer(_Int) ->
    throw({error,not_short_integer}).

%% An long integer is coded with a "short-length" followed by a
%% "multi-octet-integer"
encode_long_integer(Int) ->
    L=encode_multioctet_integer(Int,[]),
    case length(L) of
	X when X=<30 ->
	    [X]++ L
    end.

decode_long_integer([Shortlen|Content]) ->
    case Shortlen of
	Shortlen when Shortlen<31 -> % a "multi-octet-integer"
	    decode_multioctet_integer(Shortlen,Content)
    end.

%% .............................................................................
%% A "multi-octet-integer" is coded as raw data, with a maximum length of
%% 30 bytes
encode_multioctet_integer(0,Out) ->
    Out;    
encode_multioctet_integer(Int,Out) ->
    encode_multioctet_integer(Int div 256,[Int rem 256]++Out).

%% Note: The length field 
decode_multioctet_integer(Len,Content) ->
    Str=string:sub_string(Content,1,Len),
    Int=decode_multi_integer2(Str,0),
    {lists:nthtail(Len,Content),Int}.

decode_multi_integer2([],Out) ->
    Out;
decode_multi_integer2([Num],Out) ->
    decode_multi_integer2([],Num+Out);
decode_multi_integer2([Num|In],Out) ->
    decode_multi_integer2(In,256*(Num+Out)).

%% -----------------------------------------------------------------------------
%% From 8.4.2.2 WSP1.0, Length encodings
encode_valuelength(Val) ->
    case length(Val) of
	X when X>=0,X=<30 -> 
	    [X];
	X ->
	    Len=pack_uintvar(X),
	    [31] ++ Len
    end.
    
decode_valuelength(Content) ->
    case hd(Content) of
	X when X>=0,X=<30 -> 
	    {tl(Content),X};
	X when X==31 ->
	    unpack_uintvar(tl(Content))
    end.

%% -----------------------------------------------------------------------------
%% From 8.4.2.3 WSP 1.1, Parameter Values encodings
encode_text_val([]) ->
    [0];
encode_text_val(Val) ->
    case isquoted_string(Val) of
	true ->
	    encode_quoted_string(Val);
	false ->
	    encode_token_text(Val)
    end.

%% Test if a string is quoted
isquoted_string([H|Val]) ->
    case H of
	$" -> 
	    case lists:last(Val) of $" -> true;  _ ->  false end;
	_ ->
	    false
    end.

decode_text_val([0|T]) ->  {T,[]};
decode_text_val([34|T]) -> decode_quoted_string(T);
decode_text_val(T) ->      decode_token_text(T).

%% .............................................................................
%% An integer is either a "short-integer", "long-integer", defined above
encode_integer(A) when is_list(A) ->
    encode_integer(list_to_integer(A));
encode_integer(Int) when Int=<127 -> % "short-integer"
    encode_short_integer(Int);
encode_integer(Int) -> % "long-integer"
    encode_long_integer(Int).

%% An "integer" is either a "short-integer" or "long-integer"
%% The "long-integer" is coded as "multi-octet-integer"
decode_integer([H|T]) ->
    case H of
	H when H<31 -> % "long-integer"
	    decode_multioctet_integer(H,T);
	H when H>127 -> % "short-integer"
	    {T,H-128};
	_ ->
	    throw({error,bad_integer})
    end.


%% Version number 
encode_version(Version) when is_atom(Version) ->
    encode_version(atom_to_list(Version));
encode_version(Version) when is_list(Version) ->
    case content_of_list(Version) of
	{integer,Major} when 1=<Major,Major=<7 ->
	    Int=wsp_bytecodes:encode_version_data({Major,15}),
	    encode_short_integer(Int);
	{integer,_} ->
	    encode_text_string(Version);
%encode_version({Major,Minor}) when Major>=1,Major=<7,Minor>=0,Minor=<14 ->
%    Int=wsp_bytecodes:encode_version_data({Major,Minor}),
%    encode_short_integer(Int);
	{text,_} ->
	    encode_text_string(Version)
    end.

decode_version([H|T]) ->
    if
	H>=32,H=<127 -> % Text-value
	    decode_text_string([H|T]);
	H>=128 ->
	    {C1,Byte}=decode_short_integer([H|T]),
	    case wsp_bytecodes:decode_version_data(Byte) of
		{Maj,15} ->
		    {C1,Maj};
		{Maj,Min} ->
		    {C1,{Maj,Min}}
	    end;
	true ->
	    throw({error,invalid_version})	    
    end;
decode_version(_) ->
    throw({error,no_version_val}).

%% URI value should be encoded per HTTP 1.1
encode_uri(V) ->
    encode_text_string(V).

decode_uri(V) ->
    decode_text_string(V).

%% =============================================================================
%% From 8.4.2.4 WSP 1.1, Parameter encodings
%% - Untyped values are "integer" or "text" values
%% - Well-known Parameter Assignments
%%   Parmeters in WSP_WELL_KNOWN_PARAMETERS are stored as tuples
%%   {Encoding,EncodingVersion,ParameterName}
encode_untyped_val(Val) when is_integer(Val) ->
    encode_integer(Val);
encode_untyped_val(Val) when is_atom(Val) ->
    encode_text_val(atom_to_list(Val));
encode_untyped_val(Val) when is_list(Val) ->
    case content_of_list(Val) of
	{text,Val} ->
	    encode_text_val(Val);
	{integer,Int} ->
	    encode_integer(Int)
    end.

decode_untyped_val([H|T]) ->
    case H of
	0 -> % Text-value (No-value)
	    {T,[]};
	31 -> % Uintvar integer
	    throw({error,unknown_untyped_val});
	X when X>=32,X=<127 -> % Text-value
	    decode_text_val([H|T]);
	_ -> % Integer-value (short or long), ie 1=<H=<30 or 128=<H=<255
	    decode_integer([H|T])
    end;
decode_untyped_val(_) ->
    throw({error,no_parameter_val}).

%% -----------------------------------------------------------------------------
%% Parameters encodings
%% Encode parameter lists on the form {Left,Right} where Right is the assigned
%% value.
%% charset - The returned Type is either constrained (short-integer or text) or
%%     general (long-integer). But always encoded properly already.
encode_parameters(Params,EncVers) ->
    encode_parameters(Params,EncVers,[]).

encode_parameters([],_,Output) ->
    Output;
encode_parameters([{L,R}|T],EncVers,Output) ->
    EncPar=encode_parameter(L,R,EncVers),
    encode_parameters(T,EncVers,Output++EncPar).


encode_parameter(L,R,EncVers) ->
    encode_parameter(L,R,EncVers,EncVers).
    
encode_parameter(L,R,0,EncVers) ->
    case encode_parameter0(L,R,EncVers) of
	{error,no_known_encoding} ->
	    encode_text_string(L) ++ encode_untyped_val(R);
	Encoded -> 
	    Encoded
    end;
encode_parameter(L,R,1,EncVers) ->
    case encode_parameter1(L,R) of
	{error,no_known_encoding} -> encode_parameter(L,R,0,EncVers);
	Encoded -> Encoded
    end;
encode_parameter(L,R,2,EncVers) ->
    case encode_parameter2(L,R) of
	{error,no_known_encoding} -> encode_parameter(L,R,1,EncVers);
	Encoded -> Encoded
    end;
encode_parameter(L,R,3,EncVers) ->
    case encode_parameter3(L,R) of
	{error,no_known_encoding} -> encode_parameter(L,R,2,EncVers);
	Encoded -> Encoded
    end.
    
%%% All parameters whose value depends on the encoding version
encode_parameter0('differences',Right,EncVers) ->
    [16#07|encode_header_field(Right,EncVers)];
encode_parameter0('type',Right,EncVers) ->
    [16#09|encode_content_type(Right,EncVers)];
encode_parameter0(_,_,_) -> {error,no_known_encoding}.


%%% Encoding version 1.1
encode_parameter1('q',Right) ->        encode_qval_list(Right);
encode_parameter1('charset',Right) ->  [16#01|encode_charset1(Right)];
encode_parameter1('level',Right) ->    [16#02|encode_version(Right)];
encode_parameter1('type',Right) ->     [16#03|encode_integer(Right)];
encode_parameter1('name',Right) ->     [16#05|encode_text_string(Right)];
encode_parameter1('filename',Right) -> [16#06|encode_text_string(Right)];
encode_parameter1('padding',Right) ->  [16#08|encode_short_integer(Right)];
encode_parameter1(_,_) -> {error,no_known_encoding}.

%%% Encoding version 1.2
encode_parameter2('start',Right) ->    [16#0a|encode_text_string(Right)];
encode_parameter2('start-info',Right)->[16#0b|encode_text_string(Right)];
encode_parameter2(_,_) -> {error,no_known_encoding}.

%%% Encoding version 1.3
encode_parameter3('comment',Right) ->  [16#0c|encode_text_string(Right)];
encode_parameter3('domain',Right) ->   [16#0d|encode_text_string(Right)];
encode_parameter3('max-age',Right) ->  [16#0e|encode_integer(Right)];
encode_parameter3('path',Right) ->     [16#0f|encode_text_string(Right)];
encode_parameter3('secure',_Right) ->   [16#10|encode_no_value()];
encode_parameter3(_,_) -> {error,no_known_encoding}.


encode_no_value() ->
    [0].


%% .............................................................................
%% Parameters encodings
%% Decodes a parameterlist, where Content contains a list with encoded
%% parameters.
decode_parlist([]) ->
    [];
decode_parlist(Content) ->
    {C1,Par}=decode_parameter(Content),
    [Par] ++ decode_parlist(C1).

decode_parameter(Content) ->
    {Cont1,Key}=decode_parameter_field(Content),
    {Cont2,Val}=decode_parameter_val(Key,Cont1),
    {Cont2,{Key,Val}}.

decode_parameter_field([H|T]) ->
    case H of
	0 -> % Text-value (No-value)
	    {T,[]};
	31 -> % Uintvar integer
	    throw({error,unknown_parameter_field});
	X when X>=32,X=<127 -> % Text-value
	    decode_text_val([H|T]);
	_X -> % Integer-value (short or long), ie 1=<X=<30 or 128=<X=<255
	    {C,Int}=decode_integer([H|T]),
	    {C,decode_field(Int,?WSP_WELL_KNOWN_PARAMETERS)}
    end.

decode_parameter_val('q',Qval) ->
    decode_qval(Qval);
decode_parameter_val('charset',Right) ->
    decode_wellknown_charset(Right);
decode_parameter_val('level',Right) ->
    decode_version(Right);
decode_parameter_val('type',Right) ->
    decode_integer(Right);
decode_parameter_val('name',C) ->
    decode_text_string(C);
decode_parameter_val('filename',C) ->
    decode_text_string(C);
decode_parameter_val('differences',Right) ->
    decode_header_field(Right);
decode_parameter_val('padding',[Pad|C]) ->
    {C,Pad-128};
decode_parameter_val(_,C) ->
    decode_untyped_val(C).

%% .............................................................................
%% Quality values
encode_qval_list(Qval) ->    
    case encode_qval(list_to_qval(Qval)) of
	[] ->  [];
	Qct -> [16#01|Qct]
    end.


%% Quality values equal to 1 should not be sent
encode_qval(Qval) when Qval==1 ->
    [];
encode_qval(Qval) when Qval>=0,Qval=<1,trunc(100*Qval)/100==Qval ->
    [trunc(Qval*100+1)];
encode_qval(Qval) when Qval>=0,Qval=<1 ->
    pack_uintvar(trunc(Qval*1000+100)).


decode_qval([1|Content]) ->
    Qval="0",
    {Content,Qval};
decode_qval([X|Content]) when X>1,X=<100 ->
    Qval="0."++to_digits(X),
    {Content,Qval};
decode_qval(Content) ->
    {C,Qval1}=unpack_uintvar(Content),
    Qval="0."++to_digits(Qval1),   %(Qval1-100)/1000,
    {C,Qval}.


to_digits(Int) when Int =<100 ->
    case (Int-1)/10 of
	X when trunc(X)==X ->
	    [round(X)+$0];
	X ->
	    [trunc(X)+$0,round((X-trunc(X))*10)+$0]
    end;
to_digits(Int) ->
    X=(Int-100),
    Dec1=trunc(X/100)+$0,
    Dec2=trunc((X/100-trunc(X/100))*10)+$0,
    Dec3=round((X/10-trunc(X/10))*10)+$0,
    [Dec1,Dec2,Dec3].

%% -----------------------------------------------------------------------------
%% Authorisation codings
%% Note: 
%% - The header that is encoded here is received from the WAP stack and UserId,
%% Password should *not* be base64 encoded in WAP!
encode_credentials({basic,BasicCred},_) ->
    [Userid,Passwrd]=string:tokens(BasicCred,":"),
    C=[128]++encode_text_string(Userid) ++ encode_text_string(Passwrd),
    encode_valuelength(C) ++ C;
encode_credentials({AuthSch,ParList},EncVers) ->
    Epl=encode_parameters(ParList,EncVers),
    C=encode_token_text(atom_to_list(AuthSch)) ++ Epl,
    encode_valuelength(C) ++ C;
encode_credentials(_,_) ->
    throw({error,unexpected_data}).

decode_credentials(Content) ->
    {C1,Len}=decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    case hd(Str) of
	128 ->
	    {Str2,Userid}=decode_text_string(tl(Str)),
	    {[],Passwrd}=decode_text_string(Str2),
	    UidPasswd=Userid++":"++Passwrd,
	    B64UidPasswd=base64encoder:pack(UidPasswd),
	    {lists:nthtail(Len,C1),{basic,B64UidPasswd}};
	X when X>=32,X=<127 ->
	    {Str2,AuthSch}=decode_text_string(Str),
	    Parlist=decode_parlist(Str2),
	    {lists:nthtail(Len,C1),{AuthSch,Parlist}};
	_ ->
	    throw({error,unexpected_data})
    end.

encode_challenge({basic,Value},_) ->
    [Attribute,Realm]=string:tokens(Value,"="),
    "realm"=string:strip(Attribute),
    C=[128|encode_text_string(string:strip(string:strip(Realm),both,$"))],
    encode_valuelength(C) ++ C;
encode_challenge({AuthSch,ParList},EncVers) ->
    {Par2,Realm}=extract_realm(ParList,[],[]),
    Epl=encode_parameters(Par2,EncVers),
    C=encode_token_text(AuthSch) ++ encode_text_string(Realm)++ Epl,
    encode_valuelength(C) ++ C;
encode_challenge(_,_) ->
    throw({error,invalid_challenge}).

extract_realm([],Re,Out) ->
    {Out,Re};
extract_realm([{L,R}|ParList],[],Out) ->
    case atom_to_lower(L) of
	realm ->
	    {Out++ParList,string:strip(string:strip(R),both,$")};
	_ ->
	    extract_realm(ParList,[],[{L,R}]++Out)
    end;
extract_realm(_,_,_) ->
    throw({error,unexpected_data}).

atom_to_lower(Atom) ->    
    list_to_atom(string:to_lower(atom_to_list(Atom))).


decode_challenge(Content) ->
    {C1,Len}=decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    case hd(Str) of
	128 ->
	    {[],Realm}=decode_text_string(tl(Str)),
	    {lists:nthtail(Len,C1),{basic,Realm}};
	X  when X>=32,X=<127 ->
	    {Str2,AuthSch}=decode_text_string(Str),
	    {Str3,Realm}=decode_text_string(Str2),
	    Parlist=decode_parlist(Str3),	    
	    {lists:nthtail(Len,C1),{AuthSch,Realm,Parlist}}
    end.


%% .............................................................................
atom_to_integer(A) ->
    list_to_integer(atom_to_list(A)).

%% Transforms from values with type atom to a float 0<V<1 or an integer 0,1
list_to_qval(Qval) ->
    case Qval of
	"0" -> 0;
	"1" -> 1;
	V ->   list_to_float(V)
    end.

%% The quick and dirty way to solve this problem...
content_of_list(Val) ->    
    case catch list_to_integer(Val) of
	{'EXIT',_Reason} ->
	    {text,Val};
	Int ->
	    {integer,Int}    
    end.


encode_headercodepage(Page) when is_integer(Page),Page>=0,Page=<30->
    [Page];
encode_headercodepage(Page) ->
    [?SHIFT_DELIMITER] ++ Page.

%% =============================================================================
%% Makes a textstring of the formatted HTTP header value in Value
print_header_val('accept',{Mediatype,Par}) ->
    flatten(Mediatype)++print_parameter_val(Par);
print_header_val('accept-charset',{V,Par}) ->
    flatten(V)++print_parameter_val(Par);
print_header_val('accept-encoding',{V,Par}) ->
    flatten(V)++print_parameter_val(Par);
print_header_val('accept-language',{V,Par}) ->
    flatten(V)++print_parameter_val(Par);
print_header_val('accept-ranges',Value) when is_atom(Value) ->
    atom_to_list(Value);
print_header_val('allow',Value) when is_atom(Value) ->
    atom_to_list(Value);
print_header_val('authorization',{basic,B64UidPasswrd}) ->
    atom_to_list('Basic')++" "++B64UidPasswrd;
print_header_val('authorization',{AuthSch,Par}) ->
    AuthSch++" "++print_sec_parameter_val(Par);
print_header_val('cache-control',Cadir) when is_atom(Cadir) ->
    atom_to_list(Cadir);
print_header_val('cache-control',Cadir) when is_list(Cadir) ->
    Cadir;
print_header_val('cache-control',Par) ->
    print_cache_parameter_val(Par);
print_header_val('connection',Value) when is_atom(Value) ->
    atom_to_list(Value);
print_header_val('content-encoding',Value) when is_atom(Value) ->
    atom_to_list(Value);
print_header_val('content-range',{_Unit,First,Last,Length}) ->    
    "bytes "++
	integer_to_list(First)++"-"++integer_to_list(Last)++"/"++
	integer_to_list(Length);
print_header_val('content-type',{CT,Par}) ->
    flatten(CT)++print_parameter_val(Par);
print_header_val('pragma',Value) when is_atom(Value) ->
    atom_to_list(Value);
print_header_val('pragma',{L,[]})  ->
    L;
print_header_val('pragma',{L,R})  ->
    L++"="++R;
print_header_val('proxy-authenticate',{basic,Realm})  ->
    atom_to_list('Basic')++" realm=\""++Realm++"\"";
print_header_val('proxy-authenticate',{AuthSch,Realm,[]})  ->
    AuthSch++" realm=\""++Realm++"\"";
print_header_val('proxy-authenticate',{AuthSch,Realm,Par})  ->
    AuthSch++" realm=\""++Realm++"\","++print_sec_parameter_val(Par);
print_header_val('proxy-authorization',{basic,B64UidPasswrd}) ->
    atom_to_list('Basic')++" "++B64UidPasswrd;
print_header_val('proxy-authorization',{AuthSch,Par}) ->
    AuthSch++" "++print_sec_parameter_val(Par);
print_header_val('range',Value) ->
    print_range_val(Value);
print_header_val('retry-after',Value) when is_integer(Value) ->
    integer_to_list(Value);
print_header_val('www-authenticate',{basic,Realm})  ->
    atom_to_list('Basic')++" realm=\""++Realm++"\"";
print_header_val('www-authenticate',{AuthSch,Realm,[]})  ->
    AuthSch++" realm=\""++"\""++Realm;
print_header_val('www-authenticate',{AuthSch,Realm,Par})  ->
    AuthSch++" realm=\""++Realm++"\","++print_sec_parameter_val(Par);
print_header_val(_Key,Value) ->
    flatten(Value).

%% Ordinary Parameters
print_parameter_val([]) ->
    [];
print_parameter_val({L,[]}) ->
    flatten(L);
print_parameter_val({L,R}) ->
    flatten(L) ++ "=" ++ flatten(R);
print_parameter_val([{L,[]}|Val]) ->
    ";" ++ flatten(L)  ++ print_parameter_val(Val);
print_parameter_val([{L,R}|Val]) ->
    ";" ++ flatten(L) ++ "=" ++ flatten(R) ++ print_parameter_val(Val).

%% Parameters to Credential/Challenges
print_sec_parameter_val([]) ->
    [];
print_sec_parameter_val([{L,_R}|Val]) ->
    flatten(L) ++ print_sec_parameter_val2(Val).%;
%print_sec_parameter_val([{L,R}|Val]) ->
%    flatten(L) ++ "=" ++ flatten(R) ++ print_sec_parameter_val2(Val).

print_sec_parameter_val2([]) ->
    [];
print_sec_parameter_val2([{L,[]}|Val]) ->
    "," ++ flatten(L) ++ print_sec_parameter_val2(Val);
print_sec_parameter_val2([{L,R}|Val]) ->
    "," ++ flatten(L) ++ "=" ++ flatten(R) ++ print_sec_parameter_val2(Val).

%% Parameters to Cachecontrol
print_cache_parameter_val({private,R}) ->
     "private=\"" ++ print_token_list(R) ++"\"";
print_cache_parameter_val({L,[]}) ->
    flatten(L);
print_cache_parameter_val({L,R}) ->
    flatten(L) ++ "=" ++ flatten(R).

print_range_val({byte_range,First,Last}) ->
    integer_to_list(First) ++"-"++integer_to_list(Last);
print_range_val({byte_range,First}) ->
    integer_to_list(First) ++"-";
print_range_val({suffix_range,Length}) ->
    "-" ++ integer_to_list(Length).
    

print_token_list([]) ->
    [];
print_token_list([T|V]) ->
    string:to_upper(atom_to_list(T))++","++print_token_list(V).


%% A temporary solution that just will flatten all output from the header
%% encoding
flatten([]) ->
    [];
flatten(A) when is_integer(A) ->
    integer_to_list(A);
flatten(A) when is_atom(A) ->
    atom_to_list(A);
flatten(A) when is_tuple(A) ->
    List=tuple_to_list(A),
    flatten(List);
flatten([A]) when is_list(A) ->
    flatten(A);
flatten(List) ->
    case isstring(List) of
	true ->
	    List;
	_ -> 
	    flatten2(List)
    end.

flatten2([A|L]) when is_integer(A) ->
    Str=integer_to_list(A),
    Str ++ [32] ++ flatten(L);
flatten2([A|L]) when is_atom(A) ->
    Str=atom_to_list(A),
    Str ++ [32] ++ flatten(L);
flatten2([A|L]) when is_tuple(A) ->
    List=tuple_to_list(A),
    flatten(List) ++ [32] ++ flatten(L);
flatten2([A|L]) when is_list(A) ->
    List=flatten(A),
    flatten(List) ++ [32] ++ flatten(L).

isstring([]) ->
    true;
isstring([A|L]) when is_integer(A),A>=32,A<128 ->
    isstring(L);
isstring([_A|_L]) ->
    false.

