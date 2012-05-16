%%% File    : wsp_headers_page1.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Header encodings for Page 1 (default) headers
%%% Created : 25 Nov 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_headers_page1).
-author('johblo@dragon.cellpt.se').
-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wsp_headers_page1.erl,v 1.2 2001/07/09 12:38:31 johblo Exp $ ').
-modified('$Date: 2001/07/09 12:38:31 $ ').
-modified_by('$Author: johblo $ ').
-vsn("1").

%% Internal to WSP header encoding
-export([
	 encode_header/3,decode_header_field/1,decode_header_value/2
	]).


-export([single_token_field/1]).


-import(wap_common,[pack_uintvar/1,unpack_uintvar/1,
		  pack_uint8/1,unpack_uint8/1]).

-include("wsp.hrl").
-include("stacklog.hrl").

%% Encode headers
%% EncVers is the Encode version to use.
%% Begin by encoding headers withthe highest supported encoding version and
%% then go through the list. The lowest defined encode version is 1, treat
%% headers whose values are dependent on the encoding version as 0 version.
%% Note that if the same header may be encoded in two different ways, the
%% highest supported encoding is always choosen.
encode_header(Key,Val,EncVers) ->
    encode_header(Key,Val,EncVers,EncVers).

encode_header(Key,Val,0,EncVers) ->
    case encode_header0(Key,Val,EncVers) of
	{error,no_known_encoding} -> 
	    wsp_bytecodes_headers:encode_text_string(Key) ++
		wsp_bytecodes_headers:encode_text_string(Val);
	Encoded -> 
	    Encoded
    end;
encode_header(Key,Val,1,EncVers) ->
    case encode_header1(Key,Val) of
	{error,no_known_encoding} -> encode_header(Key,Val,0,EncVers);
	Encoded -> Encoded
    end;
encode_header(Key,Val,2,EncVers) ->
    case encode_header2(Key,Val) of
	{error,no_known_encoding} -> encode_header(Key,Val,1,EncVers);
	Encoded -> Encoded
    end;
encode_header(Key,Val,3,EncVers) ->
    case encode_header3(Key,Val) of
	{error,no_known_encoding} -> encode_header(Key,Val,2,EncVers);
	Encoded -> Encoded
    end;
encode_header(Key,Val,X,EncVers) ->
    encode_header(Key,Val,X-1,EncVers).

%%% All headers whose value depends on the encoding version
encode_header0('accept',Value,EncVers) ->
    V=token_field(Value),
    encode_parsed_headerlist(16#80,fun encode_accept_val/2,V,EncVers);
encode_header0('authorization',Value,EncVers) ->
    V=priv_field(Value),
    [16#87|wsp_bytecodes_headers:encode_credentials(V,EncVers)];
encode_header0('cache-control',Value,EncVers) ->
    V=parameter_field(Value),
    encode_parsed_headerlist(16#88,fun encode_cachecontrol_val/2,V,EncVers);
encode_header0('content-disposition',Value,EncVers) ->
    V=single_token_field(Value),
    [16#ae|encode_contentdisposition_val(V,EncVers)];
encode_header0('content-type',Value,EncVers) ->
    V=single_token_field(Value),
    [16#91|encode_contenttype_val(V,EncVers)];
encode_header0('pragma',Value,EncVers) ->
    V=parameter_field(Value),
    encode_parsed_headerlist(16#9f,fun encode_pragma_val/2,V,EncVers);
encode_header0('proxy-authenticate',Value,EncVers)->
    V=priv_field(Value),
    [16#a0|wsp_bytecodes_headers:encode_challenge(V,EncVers)];
encode_header0('proxy-authorization',Value,EncVers)->
    V=priv_field(Value),
    [16#a1|wsp_bytecodes_headers:encode_credentials(V,EncVers)];
encode_header0('vary',Value,EncVers) ->
    V=clean_token_field(httpd_util:to_lower(Value)),
    encode_parsed_headerlist(16#aa,fun encode_header_field/2,V,EncVers);
encode_header0('www-authenticate',Value,EncVers) ->
    V=priv_field(Value),
    [16#ad|wsp_bytecodes_headers:encode_challenge(V,EncVers)];
encode_header0(_,_,_) -> {error,no_known_encoding}.

%% Headers encoded if encoding version 1.1 is enabled
encode_header1('accept-charset',Value) ->
    V=token_field(Value),
    encode_parsed_headerlist(16#81,fun encode_acceptcharset_val/1,V);
encode_header1('accept-encoding',Value) ->
    V=token_field(Value),
    encode_parsed_headerlist(16#82,fun encode_acceptencoding_val/1,V);
encode_header1('accept-language',Value) ->
    V=token_field(Value),
    encode_parsed_headerlist(16#83,fun encode_acceptlanguage_val/1,V);
encode_header1('accept-ranges',V) ->
    [16#84|encode_acceptranges_val(V)];
encode_header1('age',V) ->
    [16#85|wsp_bytecodes_headers:encode_integer(V)];
encode_header1('allow',V) ->
    encode_parsed_headerlist(16#86,fun encode_method_val/2,V,[]);
encode_header1('connection',V) ->      [16#89|encode_connection(V)];
encode_header1('content-base',V) ->    [16#8a|encode_field_val(text,V)];
encode_header1('content-encoding',V) ->[16#8b|encode_contentencoding_val(V)];
encode_header1('content-language',V) ->[16#8c|encode_contentlanguage_val(V)];
encode_header1('content-length',Value) ->
    {V,_}=num(skip_lwsp(Value)),
    [16#8d|wsp_bytecodes_headers:encode_integer(V)];
encode_header1('content-location',V) ->
    [16#8e|wsp_bytecodes_headers:encode_text_string(V)];
encode_header1('content-md5',V) ->
    [16#8f|wsp_bytecodes_headers:encode_valuelength(V)++V];
encode_header1('content-range',Value) ->
    V=httpd_util:to_lower(Value),
    [16#90|encode_contentrange_val(V)];
encode_header1('date',Value) ->
    V=date_field(Value),
    [16#92|encode_date_val(V)];
encode_header1('etag',V) ->
    [16#93|wsp_bytecodes_headers:encode_text_string(V)];
encode_header1('expires',Value) ->
    V=dec_integer_date_field(Value),
    [16#94|encode_date_val(V)];
encode_header1('from',V) ->            [16#95|encode_field_val(text,V)];
encode_header1('host',V) ->            [16#96|encode_field_val(text,V)];
encode_header1('if-match',V) ->
    [16#98|wsp_bytecodes_headers:encode_text_string(V)];
encode_header1('if-modified-since',Value)->
    V=date_field(Value),
    [16#97|encode_date_val(V)];
encode_header1('if-none-match',V) ->
    [16#99|wsp_bytecodes_headers:encode_text_string(V)];
encode_header1('if-range',Value) ->
    V=date_field(Value),
    [16#9a|encode_if_range(V)];
encode_header1('if-unmodified-since',Value)->
    V=date_field(Value),
    [16#9b|encode_date_val(V)];
encode_header1('location',V) ->        [16#9c|encode_field_val(text,V)];
encode_header1('last-modified',Value) ->
    V=date_field(Value),
    [16#9d|encode_date_val(V)];
encode_header1('max-forwards',Value) ->
    {V,_}=num(skip_lwsp(Value)),
    [16#9e|wsp_bytecodes_headers:encode_integer(V)];
encode_header1('public',V) ->          [16#a2|encode_method(V,[])];
encode_header1('range',V) ->           [16#a3|encode_range_val(V)];
encode_header1('referer',V) ->         [16#a4|encode_field_val(text,V)];
encode_header1('retry-after',Value) ->
    V=dec_integer_date_field(Value),
    [16#a5|encode_retryafter_val(V)];
encode_header1('server',V) ->          [16#a6|encode_field_val(text,V)];
encode_header1('transfer-encoding',V)->[16#a7|encode_transferencoding_val(V)];
encode_header1('upgrade',V) ->
    [16#a8|wsp_bytecodes_headers:encode_text_string(V)];
encode_header1('user-agent',V) ->      [16#a9|encode_field_val(text,V)];
encode_header1('via',V) ->             [16#ab|encode_field_val(text,V)];
encode_header1('warning',V) ->         [16#ac|encode_warning_val(V)];
encode_header1(_,_) -> {error,no_known_encoding}.

%% Headers encoded if encoding version 1.2 is enabled
encode_header2('x-wap-application-id',V)->
    [16#af|encode_applicationid_val(V)];
encode_header2('x-wap-content-uri',V)->
    [16#b0|wsp_bytecodes_headers:encode_uri(V)];
encode_header2('x-wap-initiator-uri',V)->
    [16#b1|wsp_bytecodes_headers:encode_uri(V)];
encode_header2('accept-application',V)->
    [16#b2|encode_acceptapplication_val(V)];
encode_header2('bearer-indication',Value)->
    {V,_}=num(skip_lwsp(Value)),
    [16#b3|wsp_bytecodes_headers:encode_integer(V)];
encode_header2('push-flag',Value) ->
    {V,_}=num(skip_lwsp(Value)),
    [16#b4|wsp_bytecodes_headers:encode_short_integer(V)];
encode_header2('profile',V) ->          [16#b5|encode_field_val(text,V)];
encode_header2('profile-diff',V) ->     [16#b6|encode_profile_diff(V)];
encode_header2('profile-warning',V) ->  [16#b7|encode_profile_warning(V)];
encode_header2(_,_) -> {error,no_known_encoding}.

%% Headers encoded if encoding version 1.3 is enabled
encode_header3('expect',V) ->
    [16#b8|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('te',V) ->
    [16#b9|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('trailer',V) ->
    [16#ba|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('accept-charset',V) ->
    [16#bb|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('accept-encoding',V) ->
    [16#bc|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('cache-control',V) ->
    [16#bd|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('content-range',V) ->
    [16#be|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('x-wap-tod',V) ->
    [16#bf|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('content-id',V) ->
    [16#c0|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('set-cookie',V) ->
    [16#c1|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('cookie',V) ->
    [16#c2|wsp_bytecodes_headers:encode_integer(V)];
encode_header3('encoding-version',V) ->
    [16#c3|wsp_bytecodes_headers:encode_integer(V)];
encode_header3(_,_) -> {error,no_known_encoding}.

encode_header4('x-wap-security',V) ->
    [16#c4|wsp_bytecodes_headers:encode_integer(V)];
encode_header4(_,_) -> {error,no_known_encoding}.


encode_parsed_headerlist(Encoding,Fun,[]) ->
    [];
encode_parsed_headerlist(Encoding,Fun,[V|Rest]) ->
    [Encoding|Fun(V)] ++ encode_parsed_headerlist(Encoding,Fun,Rest).

encode_parsed_headerlist(Encoding,Fun,[],EncVers) ->
    [];
encode_parsed_headerlist(Encoding,Fun,[V|Rest],EncVers) ->
    [Encoding|Fun(V,EncVers)] ++
	encode_parsed_headerlist(Encoding,Fun,Rest,EncVers).

%encode_parsed_headerlist(Encoding,Fun,Vlist,EncVers) ->
%    lists:flatmap(fun(V) -> Fun(V,EncVers) end, Vlist).

%% -----------------------------------------------------------------------------
%% Convert binary representation to header atom
%% decode_header(integer()) -> atom()
%decode_header([Head|C]) ->
%    case decode_header(Head) of
%	{ok,shift_header_code_page} ->
%	    Page=hd(C),
%	    {newpage,Page,tl(C)};
%	{ok,{shift_header_code_page,Page}} ->
%	    {newpage,Page,C};
%	{ok,{undef_code,X}} ->
%	    case string:chr([Head|C],0) of
%		0 -> throw({error,{unknown_header_code,X}});
%		_ -> ok
%	    end,
%	    {C1,Str}=wsp_bytecodes:decode_string([Head|C]),
%	    {C1,list_to_atom(Str)};
%	Atom ->
%	    {C,Atom}
%    end;
%decode_header(?SHIFT_DELIMITER)	->
%    {ok,shift_header_code_page};
%decode_header(X) when integer(X),X>=0,X=<30 ->
%    {ok,{shift_header_code_page,X}};
%decode_header(X) when integer(X) -> {ok,{undef_code,X}};
%decode_header(X) -> throw({error,unknown_header_code}).


%% .............................................................................
%% Decode header values
decode_header_value('accept',V) ->            decode_accept_val(V);
decode_header_value('accept-charset',V) ->    decode_acceptcharset_val(V);
decode_header_value('accept-encoding',V) ->   decode_acceptencoding_val(V);
decode_header_value('accept-language',V) ->   decode_acceptlanguage_val(V);
decode_header_value('accept-ranges',V) ->     decode_acceptranges_val(V);
decode_header_value('age',V) ->
    integer_to_list(wsp_bytecodes_headers:decode_integer(V));
decode_header_value('allow',Value) ->
    V=clean_token_field(Value),
    decode_method2(V,[]);
decode_header_value('authorization',V) ->
    wsp_bytecodes_headers:decode_credentials(V);
decode_header_value('cache-control',V) ->     decode_cachecontrol_val(V);
decode_header_value('connection',V) ->        decode_connection_val(V);
decode_header_value('content-base',V) ->      decode_field_val(V);
decode_header_value('content-encoding',V) ->  decode_contentencoding_val(V);
decode_header_value('content-language',V) ->  decode_contentlanguage_val(V);
decode_header_value('content-length',V) ->
    integer_to_list(wsp_bytecodes_headers:decode_integer(V));
decode_header_value('content-location',V) ->
    wsp_bytecodes_headers:decode_text_string(V);
decode_header_value('content-md5',Content) ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    {lists:nthtail(Len,C1),Str};
decode_header_value('content-range',V) ->
    decode_contentrange_val(V);
decode_header_value('content-type',V) ->      decode_contenttype_val(V);
decode_header_value('date',V) ->              decode_date_val(V);
decode_header_value('etag',V) ->
    wsp_bytecodes_headers:decode_text_string(V);
decode_header_value('expires',V) ->           decode_date_val(V);
decode_header_value('from',V) ->              decode_field_val(V);
decode_header_value('host',V) ->              decode_field_val(V);
decode_header_value('if-modified-since',V) -> decode_date_val(V);
decode_header_value('if-match',V) ->
    wsp_bytecodes_headers:decode_text_string(V);
decode_header_value('if-none-match',V) ->
    wsp_bytecodes_headers:decode_text_string(V);
decode_header_value('if-range',V) ->          decode_if_range(V);
decode_header_value('if-unmodified-since',V) ->decode_date_val(V);
decode_header_value('last-modified',V) ->     decode_date_val(V);
decode_header_value('location',V) ->          decode_field_val(V);
decode_header_value('max-forwards',V) ->
    integer_to_list(wsp_bytecodes_headers:decode_integer(V));
decode_header_value('pragma',V) ->            decode_pragma_val(V);
decode_header_value('proxy-authenticate',V) ->
    wsp_bytecodes_headers:decode_challenge(V);
decode_header_value('proxy-authorization',V) ->
    wsp_bytecodes_headers:decode_credentials(V);
decode_header_value('public',V) ->            decode_method(V,[]);
decode_header_value('range',V) ->             decode_range_val(V);
decode_header_value('referer',V) ->           decode_field_val(V);
decode_header_value('retry-after',V) ->       decode_retryafter_val(V);
decode_header_value('server',V) ->            decode_field_val(V);
decode_header_value('transfer-encoding',V) -> decode_transferencoding_val(V);
decode_header_value('upgrade',V) ->
    wsp_bytecodes_headers:decode_text_string(V);
decode_header_value('user-agent',V) ->        decode_field_val(V);
decode_header_value('vary',V) ->
    case wsp_bytecodes_headers:decode_header_field(V) of
	{code,C1,Field} -> {C1,Field};
	{C1,Field} -> {C1,Field}
    end;
decode_header_value('via',V) ->               decode_field_val(V);
decode_header_value('warning',V) ->           decode_warning_val(V);
decode_header_value('www-authenticate',Value) ->
    V=priv_field(Value),
    wsp_bytecodes_headers:decode_challenge(V);
decode_header_value('content-disposition',V)-> decode_contentdisposition_val(V);
decode_header_value('x-wap-application-id',V)->decode_applicationid_val(V);
decode_header_value('x-wap-content-uri',V) -> decode_field_val(V);
decode_header_value('x-wap-initiator-uri',V)->decode_field_val(V);
decode_header_value('accept-application',V) ->decode_acceptapplication_val(V);
decode_header_value('bearer-indication',V) ->
    integer_to_list(wsp_bytecodes_headers:decode_integer(V));
decode_header_value('push-flag',V) ->
    integer_to_list(wsp_bytecodes_headers:decode_short_integer(V));
decode_header_value('profile',V)	   -> decode_field_val(V);
decode_header_value('profile-diff',V)	   -> decode_profile_diff(V);
decode_header_value('profile-warning',V)   -> decode_profile_warning(V);

decode_header_value(Key,Input) ->
    Value=string:sub_word(Input,1,0),
    Len=length(Value)+1,
    Input1=lists:nthtail(Len,Input),
    {Input1,Value}.


%% -----------------------------------------------------------------------------
%% Well-known Header Field Name Assignments
%% Headers in WSP_WELL_KNOWN_HEADERS are stored as tuples
%% {Encoding,EncodingVersion,HeaderFieldName}
%% Note: It is assumed that Headers encoded with other Header Code Pages than
%% the WAP specific ones (1-16) are handled elsewhere.
encode_header_field(Val,EncVers) ->
    case wsp_bytecodes_headers:encode_field(Val,EncVers,
					    ?WSP_WELL_KNOWN_PAGE1_HEADERS) of
	{ok,undef_field} ->
	    wsp_bytecodes_headers:encode_string(atom_to_list(Val));
	Code ->
	    [Code]
    end.


decode_header_field(Code) ->
    wsp_bytecodes_headers:decode_field(Code,?WSP_WELL_KNOWN_PAGE1_HEADERS).


%% -----------------------------------------------------------------------------
%% Section 8.4.2.7
%% A tuple list with {Media,ParList}, where ParList may include the Quality
%% parameter
encode_accept_val({Media,[]},EncVers) ->
    wsp_bytecodes_headers:encode_content_type(Media,EncVers);
encode_accept_val({Media,ParList},EncVers) ->
    Ect=wsp_bytecodes_headers:encode_content_type(Media,EncVers),
    case wsp_bytecodes_headers:encode_parameters(ParList,EncVers) of 
	[] ->
	    Ect;
	Epl ->
	    Out=Ect ++ Epl,
	    wsp_bytecodes_headers:encode_valuelength(Out) ++ Out
    end.

    
decode_accept_val([X|Content]) when 0=<X,X=<31 ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    case catch decode_media_range(lists:sublist(C1,1,Len)) of
	{ok,MediaRange} ->
	    {lists:nthtail(Len,C1),MediaRange};
	Reason ->
	    ?warning("Illegal Media Range in Acccept~p",
		     [Reason],decode_accept_val),
	    {lists:nthtail(Len,C1),"unknown"}
    end;
decode_accept_val([X|Content]) when X>=128 ->
    {ok,Media}=wsp_bytecodes_headers:decode_content_type(X),
    {Content,Media};
decode_accept_val(Content) ->
    wsp_bytecodes_headers:decode_extension_media(Content).

decode_media_range([X]) when X>=128 ->
    wsp_bytecodes_headers:decode_content_type(X);
decode_media_range([X|Str]) when X>=128 ->
    {ok,Media}=wsp_bytecodes_headers:decode_content_type(X),
    Parlist=wsp_bytecodes_headers:decode_parlist(Str),
    {ok,{Media,Parlist}};
decode_media_range([X|Str]) when 0=<X,X=<31 -> %There are no such known encoding
    {Str1,Int}=wsp_bytecodes_headers:decode_long_integer([X|Str]),
    Media=list_to_atom("WAP-Media-code "++integer_to_list(Int)),
    case Str1 of
	[] ->
	    {ok,Media};
	Str1 ->
	    Parlist=wsp_bytecodes_headers:decode_parlist(Str1),
	    {ok,{Media,Parlist}}
    end;
decode_media_range(Str) ->
    {Str1,Media}=wsp_bytecodes_headers:decode_extension_media(Str),
    case Str1 of
	[] ->
	    {ok,Media};
	Str1 ->
	    Parlist=wsp_bytecodes_headers:decode_parlist(Str1),
	    {ok,{Media,Parlist}}
    end.



%% .............................................................................
%% Section 8.4.2.8
encode_acceptcharset_val({'*',[]}) ->
    [128];
encode_acceptcharset_val({Charset,[]}) ->
    wsp_bytecodes_headers:encode_charset3(Charset);
encode_acceptcharset_val({'*',[{'q',Qpar}]}) ->
    case wsp_bytecodes_headers:encode_qval_list(Qpar) of
	[] ->  [128];
	Eqv -> Out=[128|Eqv],wsp_bytecodes_headers:encode_valuelength(Out)++Out
    end;
encode_acceptcharset_val({Charset,[{'q',Qpar}]}) ->
    case wsp_bytecodes_headers:encode_qval_list(Qpar) of 
	[] ->
	    wsp_bytecodes_headers:encode_charset3(Charset);
	Eqv ->
	    wsp_bytecodes_headers:encode_charset2(Charset,Eqv)
    end.


%% Constrained encoding: Text or Well-known Charset, no parameters
decode_acceptcharset_val([X|Content]) when X>=0,X=<31 -> % General form
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    case catch decode_accept_charset_general_form(lists:sublist(C1,1,Len)) of
	{ok,Charset} ->
	    {lists:nthtail(Len,C1),Charset};
	Reason ->
	    ?warning("Illegal Charset in Accept-Charset ~p",
		     [Reason],decode_acceptcharset_val),
	    {lists:nthtail(Len,C1),"unknown"}
    end;
decode_acceptcharset_val([X|Content]) when X>=32,X=<127 -> % Constrained Charset
    wsp_bytecodes_headers:decode_text_string([X|Content]);
decode_acceptcharset_val([128|Content]) -> % Constrained Charset
    {Content,'*'};
decode_acceptcharset_val(Content) -> % Constrained Charset
    {C1,Int}=wsp_bytecodes_headers:decode_short_integer(Content),
    case wsp_bytecodes_headers:decode_charset(Int) of
	{error,unknown_charset} ->
	    throw({error,unknown_charset});
	{ok,Atom} ->
	    {C1,Atom}	    
    end.

decode_accept_charset_general_form([X|Str]) when X>=32,X=<127 -> % Charset
    {Str1,Charset}=wsp_bytecodes_headers:decode_text_string([X|Str]),
    case Str1 of
	[] ->
	    {ok,Charset};
	_ ->
	    {Str2,Qval}=wsp_bytecodes_headers:decode_qval(Str1),
	    {ok,{Charset,[{'q',Qval}]}}
    end;
decode_accept_charset_general_form(Str) ->    
    {Str1,Charset}=wsp_bytecodes_headers:decode_wellknown_charset(Str),
    case Str1 of
	[] ->
	    {ok,Charset};
	_ ->
	    {Str2,Qval}=wsp_bytecodes_headers:decode_qval(Str1),
	    {ok,{Charset,[{'q',Qval}]}}
    end.

%% .............................................................................
%% Section 8.4.2.9
% This is strangely enogh not a legal encoding
% encode_acceptencoding_val({'*',[]}) -> [132];
encode_acceptencoding_val({V,[]}) ->
    encode_contentencoding_val(V);
encode_acceptencoding_val({V,[{'q',Qpar}]}) ->
    Een=encode_acceptencoding_val2(V),
    case wsp_bytecodes_headers:encode_qval_list(Qpar) of 
	[] ->
	    Een;
	Eqv ->
	    Out=Een ++ Eqv,
	    wsp_bytecodes_headers:encode_valuelength(Out) ++ Out
    end.

encode_acceptencoding_val2('*') -> [132];
encode_acceptencoding_val2(V) -> encode_contentencoding_val(V).


%% Constrained encoding: Only well-known encodings
decode_acceptencoding_val([X|Content]) when 0=<X,X=<31 ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    case decode_encoding(string:sub_string(C1,1,Len)) of
	{[],Encoding} ->
	    {lists:nthtail(Len,C1),Encoding};
	{Str1,Encoding} ->
	    {Str2,Par}=wsp_bytecodes_headers:decode_short_integer(Str1),
	    case wsp_bytecodes_headers:decode_parameter_field(Par) of
		'q' ->
		    {[],Qval}=wsp_bytecodes_headers:decode_qval(tl(Str1)),
		    {lists:nthtail(Len,C1),{Encoding,[{'q',Qval}]}};
		_ ->
		    throw({error,unknown_parameter_field})
	    end
    end;
decode_acceptencoding_val(Content) ->
    decode_contentencoding_val(Content).


decode_encoding([132|C]) -> {C,"*"};
decode_encoding(C) -> decode_contentencoding_val(C).


%% .............................................................................
%% Section 8.4.2.10
encode_acceptlanguage_val({"*",[]}) ->
    [?ANY_LANGUAGE];
encode_acceptlanguage_val({Lang,[]}) ->
    wsp_bytecodes_headers:encode_language3(Lang);
encode_acceptlanguage_val({'*',[{'q',Qpar}]}) ->
    case wsp_bytecodes_headers:encode_qval_list(Qpar) of
	[] ->  [?ANY_LANGUAGE];
	Eqv -> Out=[?ANY_LANGUAGE]++Eqv,
	       wsp_bytecodes_headers:encode_valuelength(Out)++Out
    end;
encode_acceptlanguage_val({Lang,[{'q',Qpar}]}) ->
    case wsp_bytecodes_headers:encode_qval_list(Qpar) of
	[] ->
	    wsp_bytecodes_headers:encode_language3(Lang);
	Eqv ->
	    wsp_bytecodes_headers:encode_language2(Lang,Eqv)
    end.


%% Constrained encoding: Text or Well-known Language, no parameters
decode_acceptlanguage_val([X|Content]) when X>=0,X=<31 ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    case catch decode_accept_language_general_form(lists:sublist(C1,1,Len)) of
	{ok,Lang} ->
	    {lists:nthtail(Len,C1),Lang};
	Reason ->
	    ?warning("Illegal Language in Accept-Language ~p",
		     [Reason],decode_acceptlanguage_val),
	    {lists:nthtail(Len,C1),"unknown"}
    end;
decode_acceptlanguage_val([X|Content]) when X>=32,X=<127 -> % Constrained Lang
    wsp_bytecodes_headers:decode_text_string([X|Content]);
decode_acceptlanguage_val([128|Content]) -> % Constrained Lang
    {Content,'*'};
decode_acceptlanguage_val(Content) -> % Constrained Lang
    {C1,Int}=wsp_bytecodes_headers:decode_short_integer(Content),
    case wsp_bytecodes_headers:decode_language(Int) of
	{error,unknown_language} ->
	    throw({error,unknown_language});
	{ok,Atom} ->
	    {C1,Atom}
    end.

decode_accept_language_general_form([X|Str]) when X>=32,X=<127 ->
    {Str1,Lang}=wsp_bytecodes_headers:decode_text_string([X|Str]),
    case Str1 of
	[] ->
	    {ok,Lang};
	_ ->
	    {Str2,Qval}=wsp_bytecodes_headers:decode_qval(Str1),
	    {ok,{Lang,[{'q',Qval}]}}
    end;
decode_accept_language_general_form(Str) ->    
    {Str1,Lang}=wsp_bytecodes_headers:decode_wellknown_language(Str),
    case Str1 of
	[] ->
	    {ok,Lang};
	_ ->
	    {Str2,Qval}=wsp_bytecodes_headers:decode_qval(Str1),
	    {ok,{Lang,[{'q',Qval}]}}
    end.


%% .............................................................................
%% Section 8.4.2.11
encode_acceptranges_val("none") -> [128];
encode_acceptranges_val("bytes") -> [129];
encode_acceptranges_val(V) -> encode_field_val(text,V).

decode_acceptranges_val([128|Content]) -> {Content,"none"};
decode_acceptranges_val([129|Content]) -> {Content,"bytes"};
decode_acceptranges_val(Content) ->    decode_field_val(Content).

%% .............................................................................
%% Section 8.4.2.15
%% Note that private fields with a quoted string that may have spaces in the
%% beginning and/or end is handled, but not tab
encode_cachecontrol_val({Cacheval,[]},EncVers) ->
    encode_cachecontrol_value(Cacheval);
encode_cachecontrol_val(V,EncVers) ->
    encode_cachedirective(V,EncVers).

encode_cachecontrol_value('no-cache') ->  [128];
encode_cachecontrol_value('no-store') ->  [129];
encode_cachecontrol_value('max-stale') -> [131];
encode_cachecontrol_value('only-if-cached') -> [133];
encode_cachecontrol_value('private') -> [135];
encode_cachecontrol_value('public') -> [134];
encode_cachecontrol_value('no-transform') -> [136];
encode_cachecontrol_value('must-revalidate') -> [137];
encode_cachecontrol_value('proxy-revalidate') -> [138];
encode_cachecontrol_value(V) ->
    wsp_bytecodes_headers:encode_token_text(V).

encode_cachedirective({'no-cache',FieldList},EncVers) ->
    Val=[128|encode_fields(FieldList,EncVers)],
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val;
encode_cachedirective({'max-age',DeltaSec},_) ->
    Val=[130|wsp_bytecodes_headers:encode_integer(DeltaSec)],
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val;
encode_cachedirective({'max-stale',DeltaSec},_) ->
    Val=[131|wsp_bytecodes_headers:encode_integer(DeltaSec)],
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val;
encode_cachedirective({'min-fresh',DeltaSec},_) ->
    Val=[132|wsp_bytecodes_headers:encode_integer(DeltaSec)],
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val;
encode_cachedirective({'private',FieldList},EncVers) ->
    F=string:tokens(httpd_util:tolower(string:strip(string:strip(FieldList),
						    both,$")),","),
    Fieldcodes=encode_fields(F,EncVers),
    Val=[135|Fieldcodes],
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val;
encode_cachedirective({'s-maxage',DeltaSec},EncVers) when EncVers>2 ->
    Val=[139|wsp_bytecodes_headers:encode_integer(DeltaSec)],
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val;
encode_cachedirective({L,R},EncVers) ->
    Val=wsp_bytecodes_headers:encode_parameter(L,R,EncVers),
    wsp_bytecodes_headers:encode_valuelength(Val) ++ Val.


decode_cachecontrol_val([X|Content]) when X>=0,X=<31 ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    Str=string:sub_string(C1,1,Len),
    Dirs=decode_cachedirective(Str),
    {lists:nthtail(Len,C1),Dirs};	    
decode_cachecontrol_val([X|Content]) when X>=32,X=<127 ->
    wsp_bytecodes_headers:decode_token_text([X|Content]);
decode_cachecontrol_val([128|Content]) -> {Content,'no-cache'};
decode_cachecontrol_val([129|Content]) -> {Content,'no-store'};
decode_cachecontrol_val([131|Content]) -> {Content,'max-stale'};
decode_cachecontrol_val([132|Content]) -> {Content,'min-fresh'};
decode_cachecontrol_val([133|Content]) -> {Content,'only-if-cached'};
decode_cachecontrol_val([134|Content]) -> {Content,public};
decode_cachecontrol_val([135|Content]) -> {Content,private};
decode_cachecontrol_val([136|Content]) -> {Content,'no-transform'};
decode_cachecontrol_val([137|Content]) -> {Content,'must-revalidate'};
decode_cachecontrol_val([138|Content]) -> {Content,'proxy-revalidate'}.


decode_cachedirective([128|Content]) -> {'no-cache',decode_fields(Content)};
decode_cachedirective([130|Content]) -> {'max-age',decode_integer(Content)};
decode_cachedirective([131|Content]) -> {'max-stal',decode_integer(Content)};
decode_cachedirective([132|Content]) -> {'min-fresh',decode_integer(Content)};
decode_cachedirective([135|Content]) -> {private,decode_fields(Content)};
decode_cachedirective([X|Content]) when X>=32,X=<127 ->
    {C1,Ext}=wsp_bytecodes_headers:decode_token_text([X|Content]),
    {C2,Par}=wsp_bytecodes_headers:decode_untyped_val(C1),
    {Ext,Par}.

decode_integer(Content) ->
    {[],Val}=wsp_bytecodes_headers:decode_integer(Content),
    Val.

%% .............................................................................
%% Section 8.4.2.16
encode_connection("close") -> [128];
encode_connection(V) -> encode_field_val(text,V).

decode_connection_val(V) ->
    {C,Val}=decode_field_val(V),
    Val2=case Val of
	     128 -> close;
	     X -> X    
	 end,
    {C,Val2}.

%% .............................................................................
%% Section 8.4.2.18
encode_contentencoding_val(gzip) ->     [128];
encode_contentencoding_val(compress) -> [129];
encode_contentencoding_val(deflate) ->  [130];
encode_contentencoding_val(identity) -> [131];
encode_contentencoding_val(V) -> wsp_bytecodes_headers:encode_token_text(V).

decode_contentencoding_val([128|Content]) -> {Content,gzip};
decode_contentencoding_val([129|Content]) -> {Content,compress};
decode_contentencoding_val([130|Content]) -> {Content,deflate};
decode_contentencoding_val([131|Content]) -> {Content,identity};
decode_contentencoding_val([X|Content]) when X>=32,X=<127 ->
    wsp_bytecodes_headers:decode_token_text([X|Content]).

%% .............................................................................
%% Section 8.4.2.19
encode_contentlanguage_val('*') -> [?ANY_LANGUAGE];
encode_contentlanguage_val(V) ->   wsp_bytecodes_headers:encode_language1(V).

decode_contentlanguage_val([X|Content]) when X>=32,X=<127 ->
    wsp_bytecodes_headers:decode_token_text([X|Content]);
decode_contentlanguage_val(Content) ->
    wsp_bytecodes_headers:decode_wellknown_language(Content).

%% .............................................................................
%% Section 8.4.2.23
%% HTTP 1.1 (RFC 2616) rules
%%   Content-Range = "Content-Range" ":" content-range-spec
%%   content-range-spec      = byte-content-range-spec
%%   byte-content-range-spec = bytes-unit SP
%%                                  byte-range-resp-spec "/"
%%                                  ( instance-length | "*" )
%%   byte-range-resp-spec = (first-byte-pos "-" last-byte-pos)
%%                                       | "*"
%%   instance-length           = 1*DIGIT

%% Note:
%% - This is the byte position or total length of the uncoded PDU
%%   However, this is NOT correct if for example the actual data sent is
%%   bytecoded WML instead of textutal WML.
encode_contentrange_val([$b,$y,$t,$e,$s|Value]) ->
    Val=skip_lwsp(Value),
    Range=string:sub_word(Val,1,$/),
    Length=list_to_integer(string:sub_word(skip_lwsp(Val),2,$/)),
    First=list_to_integer(string:substr(Range,1,string:chr(Range,$-)-1)),
    C=pack_uintvar(First)++pack_uintvar(Length),
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
encode_contentrange_val(_) ->
    throw({error,unexpected_data}).


decode_contentrange_val(Content) ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    {Str1,First}=unpack_uintvar(Str),
    {[],Length}=unpack_uintvar(Str1),
    Last=First+Length,
    {lists:nthtail(Len,C1),{bytes,First,Last,Length}}.

%% .............................................................................
%% Section 8.4.2.24
encode_contenttype_val({V,ParList},EncVers) ->
    C=wsp_bytecodes_headers:encode_content_type(V,EncVers)++
	wsp_bytecodes_headers:encode_parameters(ParList,EncVers),
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
encode_contenttype_val(V,EncVers) ->
    wsp_bytecodes_headers:encode_content_type(V,EncVers).
    
%% General form: Text or Well-known Content type, with parameters
%% Constrained encoding: Text or Well-known Content type, no parameters
%% Note: Media-range is identically defined as Media-type
decode_contenttype_val([X|Content]) when 0=<X,X=<31 ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    case catch decode_media_range(lists:sublist(C1,1,Len)) of
	{ok,MediaRange} ->
	    {lists:nthtail(Len,C1),MediaRange};
	Reason ->
	    ?warning("Illegal Media Range in Content-Type ~p",
		     [Reason],decode_acceptcharset_val),
	    {lists:nthtail(Len,C1),"unknown"}
    end;
decode_contenttype_val([X|Content]) when X>=128 ->
    {ok,Media}=wsp_bytecodes_headers:decode_content_type(X),
    {Content,Media};
decode_contenttype_val(Content) ->
    wsp_bytecodes_headers:decode_extension_media(Content).

%% .............................................................................
%% Section 8.4.2.25
%% Note that this is just a fast hack to get date encoding work...
%% The real thing should'nt be limited by 
encode_date_val(V) when tuple(V) ->
    {Days,Time}=calendar:time_difference({{1970,1,1},{0,0,0}},V),
    Dayssec=Days*86400,
    Timesec=calendar:time_to_seconds(Time),
    Secs=Dayssec+Timesec,
    wsp_bytecodes_headers:encode_long_integer(Secs);
encode_date_val(_) ->
    throw({error,unexpected_data}).

%% calendar:date_to_gregorian_days({1970,1,1}) = 719528
decode_date_val(Content) ->
    {C1,Secs}=wsp_bytecodes_headers:decode_long_integer(Content),
    {Days,Time}=calendar:seconds_to_daystime(Secs),
    Date=calendar:gregorian_days_to_date(719528+Days),
    {C1,enc_date({Date,Time})}.

%% .............................................................................
%% Section 8.4.2.33
encode_if_range(V) when tuple(V) -> encode_date_val(V);
encode_if_range(V) -> wsp_bytecodes_headers:encode_text_string(V).

decode_if_range([A|Content]) when 0=<A,A<31 ->
    decode_date_val([A|Content]);
decode_if_range(Content) ->
    wsp_bytecodes_headers:decode_text_string(Content).

%% .............................................................................
%% Section 8.4.2.38
encode_pragma_val('no-cache',EncVers) ->
    [128];
encode_pragma_val({L,R},EncVers) ->
    C=wsp_bytecodes_headers:encode_parameter(L,R,EncVers),
    wsp_bytecodes_headers:encode_valuelength(C) ++ C.

decode_pragma_val([128|Content]) ->
    {Content,'no-cache'};
decode_pragma_val(Content) ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    {[],Par}=wsp_bytecodes_headers:decode_parameter(Str),
    {lists:nthtail(Len,C1),Par}.

%% .............................................................................
%% Section 8.4.2.42
%% HTTP 1.1 (RFC 2616) rules:
%%   Range = "Range" ":" ranges-specifier
%%   range-unit       = bytes-unit | other-range-unit
%%   bytes-unit       = "bytes"
%%   other-range-unit = token
%%   ranges-specifier = byte-ranges-specifier
%%   byte-ranges-specifier = bytes-unit "=" byte-range-set
%%   byte-range-set  = 1#( byte-range-spec | suffix-byte-range-spec )
%%   byte-range-spec = first-byte-pos "-" [last-byte-pos]
%%   first-byte-pos  = 1*DIGIT
%%   last-byte-pos   = 1*DIGIT
%%   suffix-byte-range-spec = "-" suffix-length
%%   suffix-length = 1*DIGIT

encode_range_val({bytes,Range}) ->
    encode_range_val2(string:tokens(Range,",")).

encode_range_val2([]) ->
    [];
encode_range_val2([Range|T]) ->
    case string:chr(Range,$-) of
	1 ->
	    Length=list_to_integer(skip_lwsp(tl(Range))),
	    encode_range_val3({suffix_range,Length}) ++ encode_range_val2(T);
	X when X>0 ->
	    First=list_to_integer(string:substr(Range,1,X-1)),
	    case skip_lwsp(lists:nthtail(X,Range)) of
		[] ->
		    encode_range_val3({byte_range,First})++encode_range_val2(T);
		Last ->
		    encode_range_val3({bytes,First,list_to_integer(Last)})
			++ encode_range_val2(T)
	    end
    end.

encode_range_val3({bytes,First,Last}) ->
    C=[128|pack_uintvar(First)]++pack_uintvar(Last),
    wsp_bytecodes_headers:encode_valuelength(C)++C;
encode_range_val3({byte_range,First}) ->
    C=[128|pack_uintvar(First)],
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
encode_range_val3({suffix_range,Length}) ->
    C=[129|pack_uintvar(Length)],
    wsp_bytecodes_headers:encode_valuelength(C) ++ C.


decode_range_val(Content) ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    case hd(Str) of
	128 ->
	    {Str2,First}=unpack_uintvar(tl(Str)),
	    case Str2 of
		[] ->
		    {lists:nthtail(Len,C1),{byte_range,First}};
		_ ->
		    {[],Last}=unpack_uintvar(Str2),
		    {lists:nthtail(Len,C1),{byte_range,First,Last}}
	    end;
	129 ->
	    {[],Length}=unpack_uintvar(tl(Str)),
	    {lists:nthtail(Len,C1),{suffix_range,Length}}
    end.

%% .............................................................................
%% Section 8.4.2.44
%% Note: The value length field is not used !
encode_retryafter_val(Date) when tuple(Date) ->
    C=[128]++encode_date_val(Date),
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
encode_retryafter_val(DeltaSec) ->
    C=[129]++wsp_bytecodes_headers:encode_integer(DeltaSec),
    wsp_bytecodes_headers:encode_valuelength(C) ++ C.

decode_retryafter_val(Content) ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    case hd(C1) of
	128 ->
	    {[],Date}=decode_date_val(tl(Str)),
	    {lists:nthtail(Len,C1),Date};
	129 ->
	    {[],Time}=wsp_bytecodes_headers:decode_integer(tl(Str)),
	    {lists:nthtail(Len,C1),integer_to_list(Time)}
    end.


%% .............................................................................
%% Section 8.4.2.46
encode_transferencoding_val("chunked") -> [128];
encode_transferencoding_val(V) ->
    wsp_bytecodes_headers:encode_token_text(V).

decode_transferencoding_val([128|V]) ->
    {V,"chunked"};
decode_transferencoding_val(V) ->
    wsp_bytecodes_headers:decode_token_text(V).


%% .............................................................................
%% Section 8.4.2.51
%% HTTP 1.1 (RFC 2616) rules:
%%       Warning    = "Warning" ":" 1#warning-value
%%       warning-value = warn-code SP warn-agent SP warn-text
%%                                             [SP warn-date]
%%       warn-code  = 3DIGIT
%%       warn-agent = ( host [ ":" port ] ) | pseudonym
%%                       ; the name or pseudonym of the server adding
%%                       ; the Warning header, for use in debugging
%%       warn-text  = quoted-string
%%       warn-date  = <"> HTTP-date <">

encode_warning_val([D1,D2,D3|Value]) ->
    Code=list_to_integer([D1,D2,D3]),
    Val=skip_lwsp(Value),
    case string:tokens(Val," ") of
	[Agent,Text] ->
	    C=wsp_bytecodes_headers:encode_short_integer(Code)++
		wsp_bytecodes_headers:encode_text_string(Agent)++
		wsp_bytecodes_headers:encode_text_string(Text),
	    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
	WarningVal-> %% May include Date also
	    {Text,Agent}=extract_word(Val,[]),
	    C=wsp_bytecodes_headers:encode_short_integer(Code)++
		wsp_bytecodes_headers:encode_text_string(Agent)++
		wsp_bytecodes_headers:encode_text_string(Text),
	    wsp_bytecodes_headers:encode_valuelength(C) ++ C
    end.

extract_word([$\t |Word],Out) ->
    {Out,skip_lwsp(Word)};
extract_word([$ |Word],Out) ->
    {Out,skip_lwsp(Word)};
extract_word([X|Word],Out) ->
    extract_word(Word,[X|Out]).

decode_warning_val([X|Content]) when X>=0,X=<31 ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength([X|Content]),
    Str=string:sub_string(C1,1,Len),
    {Str2,Code}=wsp_bytecodes_headers:decode_short_integer(Str),
    {Str3,Agent}=wsp_bytecodes_headers:decode_text_string(Str2),
    {[],Text}=wsp_bytecodes_headers:decode_text_string(Str3),
    {lists:nthtail(Len,C1),{Code,Agent,Text}};
decode_warning_val([X|Content]) when X>=128 ->
    {Content,X-128}.

%% .............................................................................
%% Section 8.4.2.53
encode_contentdisposition_val({form,ParList},EncVers) ->
    C=[128| wsp_bytecodes_headers:encode_parameters(ParList,EncVers)],
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
encode_contentdisposition_val({attach,ParList},EncVers) ->
    C=[129| wsp_bytecodes_headers:encode_parameters(ParList,EncVers)],
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;

%%% Only in Encoding Version 1.4
encode_contentdisposition_val({inline,ParList},EncVers) ->
    C=[130| wsp_bytecodes_headers:encode_parameters(ParList,EncVers)],
    wsp_bytecodes_headers:encode_valuelength(C) ++ C;
encode_contentdisposition_val({Text,ParList},EncVers) ->
    C=wsp_bytecodes_headers:encode_token_text(Text)++
	wsp_bytecodes_headers:encode_parameters(ParList,EncVers),
    wsp_bytecodes_headers:encode_valuelength(C) ++ C.

decode_contentdisposition_val(Content) ->
    {C1,Len}=wsp_bytecodes_headers:decode_valuelength(Content),
    Str=string:sub_string(C1,1,Len),
    case hd(Str) of
	128 ->
	    Parlist=wsp_bytecodes_headers:decode_parlist(tl(Str)),
	    {lists:nthtail(Len,C1),{form,Parlist}};
	129 ->
	    Parlist=wsp_bytecodes_headers:decode_parlist(tl(Str)),
	    {lists:nthtail(Len,C1),{attach,Parlist}}
    end.

%% .............................................................................
%% Section 8.4.2.54
encode_applicationid_val([X|V]) when X>=32,X=<127 ->
    wsp_bytecodes_headers:encode_text_string([X|V]);
encode_applicationid_val(V) ->
    wsp_bytecodes_headers:encode_integer(V).

decode_applicationid_val([X|Content]) when X>=32,X=<127 ->
    wsp_bytecodes_headers:decode_text_string([X|Content]);
decode_applicationid_val(Content) ->
    wsp_bytecodes_headers:decode_integer(Content).

%% .............................................................................
%% Section 8.4.2.57
encode_acceptapplication_val('*') -> [128];
encode_acceptapplication_val(V) -> encode_applicationid_val(V).

decode_acceptapplication_val([128|Content]) -> {'*',Content};
decode_acceptapplication_val(V) -> decode_applicationid_val(V).

%% -----------------------------------------------------------------------------
%% Note: Methods are case-sensitive!
encode_method(V,EMlist) -> % No Negotiated Extended Methods !
    case from_method(V,EMlist) of
	{no_method,A} ->
	    encode_field_val(text,A);
	A ->
	    wsp_bytecodes_headers:encode_short_integer(A)
    end.

encode_method_val(V,EMlist) -> % No Negotiated Extended Methods !
    case from_method(V,EMlist) of
	{no_method,A} ->
	    throw({error,invalid_method});
	A ->
	    wsp_bytecodes_headers:encode_short_integer(A)
    end.

decode_method(Content,EMlist) ->
    case hd(Content) of
	X when X>=32,X=<127 ->
	    Str=string:sub_word(Content,1,0),
	    {lists:nthtail(length(Str)+1,Content),Str};
	X when X>=128 ->
	    decode_method2(Content,EMlist)
    end.

decode_method2([X|Content],EMlist) ->
    Val=to_method(X-128,EMlist),
    {Content,Val}.

to_method(?Get,_) -> 'GET';
to_method(?Options,_) -> 'OPTIONS';
to_method(?Head,_) -> 'HEAD';
to_method(?Delete,_) -> 'DELETE';
to_method(?Trace,_) -> 'TRACE';
to_method(X,EMlist) when integer(X),X>=80,X=<95 -> X;
to_method(?Post,_) -> 'POST';
to_method(?Put,_) -> 'PUT';
to_method(X,_) when integer(X),X>=112,X=<127 -> X;
to_method(A,_) ->
    throw({error,invalid_typecode}).

from_method('GET',_) -> ?Get;
from_method('OPTIONS',_) -> ?Options;
from_method('HEAD',_) -> ?Head;
from_method('DELETE',_) -> ?Delete;
from_method('TRACE',_) -> ?Trace;
from_method('POST',_) -> ?Post;
from_method('PUT',_) -> ?Put;
from_method(A,EMlist) -> % No Negotiated Extended Methods !
    {no_method,A}.

%% .............................................................................
encode_profile_diff(V) ->
    [].

decode_profile_diff(Content) ->
    {[],Content}.

%% .............................................................................
encode_profile_warning({Code,Target,Date}) -> % OBS BUG
    C=encode_warning_code(Code)++
	wsp_bytecodes_headers:encode_uri(Target)++encode_date_val(Date),
    wsp_bytecodes_headers:encode_valuelength(C) ++C;
encode_profile_warning(V) when integer(V) ->
    encode_warning_code(V).

decode_profile_warning([Code|Content]) -> % OBS BUG
    decode_warning_code(Code,Content).


encode_warning_code(100) -> [16#90];
encode_warning_code(101) -> [16#91];
encode_warning_code(102) -> [16#92];
encode_warning_code(200) -> [16#a0];
encode_warning_code(201) -> [16#a1];
encode_warning_code(202) -> [16#a2];
encode_warning_code(203) -> [16#a3].


decode_warning_code(16#90,Content) -> {Content,100};
decode_warning_code(16#91,Content) -> {Content,101};
decode_warning_code(16#92,Content) -> {Content,102};
decode_warning_code(16#a0,Content) -> {Content,200};
decode_warning_code(16#a1,Content) -> {Content,201};
decode_warning_code(16#a2,Content) -> {Content,202};
decode_warning_code(16#a3,Content) -> {Content,203};
decode_warning_code(16#a4,Content) -> throw({error,unknown_warning_code}).


encode_fields([],_) ->
    [];
encode_fields([Field | T],EncVers) ->
    encode_header_field(list_to_atom(Field),EncVers)++encode_fields(T,EncVers).

decode_fields([]) ->
    [];
decode_fields(Content) ->
    case wsp_bytecodes_headers:decode_header_field(Content) of
	{code,C1,Headers} ->	
	    [Headers]++decode_fields(C1);
	{C1,Headers} ->
	    [Headers]++decode_fields(C1)
    end.


%% -----------------------------------------------------------------------------
clean_token_field(Value) ->
    lists:map(
      fun(Token) ->list_to_atom(skip_lwsp(Token)) end,string:tokens(Value,",")).

token_field(Value) ->
    lists:map(
      fun(Token) -> single_token_field(Token) end, string:tokens(Value, ",")).

single_token_field(Value) ->
    [Token | Params] = string:tokens(Value, ";"),
    {list_to_atom(skip_lwsp(Token)),parameter_field_list(skip_lwsp(Params))}.
    
parameter_field(Value) ->
    parameter_field_list(string:tokens(Value, ",")).

parameter_field_list(Params) -> 
    lists:map(
      fun(Param) -> single_parameter_field(Param) end, Params).

single_parameter_field(Param) -> 
    case string:tokens(Param, "=") of
	[Attribute,Value] ->
	    {list_to_atom(skip_lwsp(Attribute)),skip_lwsp(Value)};
	[Attribute] ->       
	    {list_to_atom(skip_lwsp(Attribute)),[]};
	Error ->
	    {list_to_atom(skip_lwsp(lists:concat(Error))),[]}
    end.

%%% ............................................................................
priv_field(Value) ->
    [Scheme|Rest]=string:tokens(Value," "),
    case list_to_atom(Scheme) of
	basic ->
	    {basic,hd(Rest)};
	AuthScheme ->
	    {AuthScheme,parameter_field(lists:concat(Rest))}
    end.

%%% ............................................................................
%%% Different combinations of the date field value 
dec_integer_date_field(Value) ->
    case catch num(skip_lwsp(Value)) of
	{'EXIT', _} -> date_field(Value);
	{Sec,_} -> Sec
    end.

date_field(Value) ->
    case catch dec_date(Value) of
	{'EXIT',_} -> Value;
	Date -> Date
    end.


%%% ----------------------------------------------------------------------------
dec_date(Line) ->
    dec_http_date(httpd_util:to_lower(Line)).

dec_http_date("monday "++Cs) -> dec_date2(Cs);
dec_http_date("tuesday "++Cs) -> dec_date2(Cs);
dec_http_date("wednesday "++Cs) -> dec_date2(Cs);
dec_http_date("thursday "++Cs) -> dec_date2(Cs);
dec_http_date("friday "++Cs) -> dec_date2(Cs);
dec_http_date("saturday "++Cs) -> dec_date2(Cs);
dec_http_date("sunday "++Cs) -> dec_date2(Cs);
dec_http_date("mon"++[X | Cs]) -> dec_date13(X,Cs);
dec_http_date("tue"++[X | Cs]) -> dec_date13(X,Cs);
dec_http_date("wed"++[X | Cs]) -> dec_date13(X,Cs);
dec_http_date("thu"++[X | Cs]) -> dec_date13(X,Cs);
dec_http_date("fri"++[X | Cs]) -> dec_date13(X,Cs);
dec_http_date("sat"++[X | Cs]) -> dec_date13(X,Cs);
dec_http_date("sun"++[X | Cs]) -> dec_date13(X,Cs).

dec_date13($ , Cs) -> dec_date3(Cs);
dec_date13($,, [$ |Cs]) -> dec_date1(Cs).

%% date1
dec_date1([D1,D2,$ ,M1,M2,M3,$ ,Y1,Y2,Y3,Y4,$  | Cs]) ->
    M = dec_month([M1,M2,M3]),
    D = list_to_integer([D1,D2]),
    Y = list_to_integer([Y1,Y2,Y3,Y4]),
    {Time," gmt"++Cs1} = dec_time(Cs),
    {{Y,M,D},Time}.

%% date2
dec_date2([D1,D2,$-,M1,M2,M3,$-,Y1,Y2 | Cs]) ->
    M = dec_month([M1,M2,M3]),
    D = list_to_integer([D1,D2]),
    Y = 1900 + list_to_integer([Y1,Y2]),
    {Time, " gmt"++Cs1} = dec_time(Cs),
    {{Y,M,D}, Time}.

%% date3
dec_date3([M1,M2,M3,$ ,D1,D2,$ | Cs]) ->
    M = dec_month([M1,M2,M3]),
    D = if D1 == $  -> list_to_integer([D2]);
	   true -> list_to_integer([D1,D2])
	end,
    {Time,[$ ,Y1,Y2,Y3,Y4|Cs1]} = dec_time(Cs),
    Y = list_to_integer([Y1,Y2,Y3,Y4]),
    {{Y,M,D}, Time}.

%% decode lowercase month
dec_month("jan") -> 1;
dec_month("feb") -> 2;
dec_month("mar") -> 3;
dec_month("apr") -> 4;
dec_month("may") -> 5;
dec_month("jun") -> 6;
dec_month("jul") -> 7;
dec_month("aug") -> 8;
dec_month("sep") -> 9;
dec_month("oct") -> 10;
dec_month("nov") -> 11;
dec_month("dec") -> 12.

%% decode time HH:MM:SS
dec_time([H1,H2,$:,M1,M2,$:,S1,S2|Cs]) ->
    { {list_to_integer([H1,H2]), 
       list_to_integer([M1,M2]),
       list_to_integer([S1,S2]) }, Cs}.


%% encode date into rfc1123-date (must be a GMT time!!!)
enc_date({{Y,M,D},{TH,TM,TS}}) ->
    WkDay = case calendar:day_of_the_week({Y,M,D}) of
		1 -> "Mon";
		2 -> "Tue";
		3 -> "Wed";
		4 -> "Thu";
		5 -> "Fri";
		6 -> "Sat";
		7 -> "Sun"
	    end,
    lists:flatten(io_lib:format("~s, ~2..0w ~s ~4..0w "
				"~2..0w:~2..0w:~2..0w GMT",
				[WkDay, D, enc_month(M), Y, TH, TM, TS])).

%% decode lowercase month
enc_month(1) -> "Jan";
enc_month(2) -> "Feb";
enc_month(3) -> "Mar";
enc_month(4) -> "Apr";
enc_month(5) -> "May";
enc_month(6) -> "Jun";
enc_month(7) -> "Jul";
enc_month(8) -> "Aug";
enc_month(9) -> "Sep";
enc_month(10) -> "Oct";
enc_month(11) -> "Nov";
enc_month(12) -> "Dec".


%% Return 1*DIGIT as a number
num([C|Cs]) when $0=<C,C=<$9 -> num(Cs,C-$0).

num([C|Cs],N) when $0=<C,C=<$9 -> num(Cs,N*10+(C-$0));
num(Cs,N) -> {N,Cs}.

%% skip space & tab
skip_lwsp([$ | Cs]) -> skip_lwsp(Cs);
skip_lwsp([$\t | Cs]) -> skip_lwsp(Cs);
skip_lwsp(Cs) -> Cs.


%% -----------------------------------------------------------------------------
%% The length of the field values are encoded according to the table on page 72,
%% Section 8.4.1.2
encode_field_val(T,C) ->
    case T of
	long ->
	    [length(C)]++C;
	uintvar ->
	    pack_uintvar(length(C)) ++ C;
	text when atom(C) ->
	    atom_to_list(C) ++ [0];
	text ->
	    C ++ [0];
	short_integer when C>=0,C<128 ->
	    [C+128];
	short_integer when C>127,C<256 ->
	    [C]
    end.

%% The length of the field values are encoded according to the table on page 72,
%% Section 8.4.1.2
decode_field_val(C) ->
    T=hd(C),
    case T of
	T when T>=0,T=<30 -> % long;
	    {C1,Data}=wap_common:unpack_data(tl(C),T);
	T when T==31 ->	% uintvar;
	    {C1,Data}=unpack_uintvar(tl(C));
	T when T>=32,T=<127 -> % text;
	    Str=string:sub_word(C,1,0),
	    {lists:nthtail(length(Str)+1,C),Str};
	T when T>=128,T=<255 -> % short_integer
	    {tl(C),T}
    end.
