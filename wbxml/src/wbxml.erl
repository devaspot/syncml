-module(wbxml).
-author('maxim@synrc.com').
-copyright('Copyright (c) 2010 Synrc Research Center').

% WBXML modular decoder/encoder with tag-pages and unicode support.
% Authors: Johan Blom <johblo@dragon.cellpt.se> and Maxim Sokhatsky <maxim@synrc.com>
% Supported formats: WML, SyncML, MetInf, DevInf.
%
% usage:
%
%     wbxml:decode(list()) -> {ok, XML} | {error, Reason}

-export([encode/4,decode/1,add_string/2,lookup_index/2,get_bin/2]).

-include("xmerl.hrl").
-include("wbxml_log.hrl").
-include("wbxml_bytecodes.hrl").

%% User State for xmerl scanning
-record(user_state,{
      str_table, % (ets) The Global String table
      module,    % (atom) Main callback module, possibly inheriting others
      wbxml,     % (uint8) WBXML version to be used for encoding
      xml,       % (#xmlDecl{}) XML declaration
      charset,   % (atom) Charset used to encode all text messages
      cbs        % (list) Call back modules, set when fetching DTD
     }).

-export([pack_uint32/1,unpack_uint32/1,
     pack_uint16/1,unpack_uint16/1,
     pack_uint8/1,unpack_uint8/1,
     pack_uintvar/1,unpack_uintvar/1,
     unpack_data/1,unpack_data/2
    ]).


% ------------------------------------------------------------------------------
%% Pack a number into upto 5 bytes, with 7 bits each.
pack_uintvar(Num) when Num<128->
    [Num];
pack_uintvar(Num) ->
    pack_uintvar(Num,[],1).
pack_uintvar(Num,_,6) ->
    throw({error,{too_big_num,Num}});
pack_uintvar(0,Out,_) ->
    Out;
pack_uintvar(Num,Out,S) ->
    Rest=Num div 128,
    Byte=if
         S==1 ->
         Num rem 128;
         true ->
         (Num rem 128) bor 128
     end,
    pack_uintvar(Rest,[Byte]++Out,S+1).


unpack_uintvar(Content) ->
    unpack_uintvar(Content,0,1).
unpack_uintvar(_,Num,6) ->
    throw({error,{too_big_num,Num}});
unpack_uintvar([Byte|In],Out,S) ->
    if
    Byte>=128 ->
        Num=Byte band 127,
        unpack_uintvar(In,128*(Num+Out),S+1);
    true ->
        {In,Byte+Out}
    end.

%% .............................................................................
% Pack integer on 1 bytes
pack_uint8(Num) ->
    pack_num(1,Num,[]).

% Pack integer on 2 bytes
pack_uint16 (Num) ->
    pack_num(2,Num,[]).

% Pack integer on 4 bytes
pack_uint32(Num) ->
    pack_num(4,Num,[]).

pack_num(0,_,List) -> List;
pack_num(Num,Sum,List) ->
    Byte=Sum rem 256,
    pack_num(Num-1,Sum div 256,[Byte|List]).

% Unpack integer on 1 bytes
unpack_uint8(Content) ->
    Num=unpack_num(1,0,Content),
    {tl(Content),Num}.

% Unpack integer on 2 bytes
unpack_uint16(Content) ->
    Num=unpack_num(2,0,Content),
    {lists:nthtail(2,Content),Num}.

% Unpack integer on 4 bytes
unpack_uint32(Content) ->
    Num=unpack_num(4,0,Content),
    {lists:nthtail(4,Content),Num}.

unpack_num(0,Sum,_) ->Sum;
unpack_num(Num,Sum,Content) ->
    unpack_num(Num-1,Sum*256+hd(Content),tl(Content)).

% ------------------------------------------------------------------------------
unpack_data(Content) ->
    {[],Content}.
unpack_data(Content,Len) when Len==0 ->
    {Content,[]};
unpack_data(Content,Len) ->
    {lists:nthtail(Len,Content),string:sub_string(Content,1,Len)}.



%%% Usage of the various xmerl_scan functions
%% fetch_fun - to catch the DTD data
%% acc_fun - to accumulate the WBXML code
%% hook_fun - to make call back to specific module supporting the DTD
%% event_fun - to store the XML declaration record
%%% See manual for explanations on character translating scheme!
encode(Content,Accept,ExtCharset,DTDRules) ->
    {Mode,InCharset,C1}=wae_man:detect_charset(ExtCharset,Content),

    Acc=fun(X,Acc,S) -> {[X|Acc], S} end,
    Fetch=fun(DTDSpec,S) ->
          case DTDSpec of
              {public,PI,URI} ->
              US=xmerl_scan:user_state(S),
              {Wbxml,Mod}=get_module(PI),
              US1=US#user_state{cbs=xmerl:callbacks(Mod),
                        module=Mod,wbxml=Wbxml},
              {ok, xmerl_scan:user_state(US1,S)};
              Other ->
              {ok, S}
          end
      end,
    Hook=fun(ParsedEntity, S) ->
         US=xmerl_scan:user_state(S),
         WBXMLinfo=#wbxml_info{str_table=US#user_state.str_table,
                       charset=US#user_state.charset},
         case xmerl:export_element(ParsedEntity,US#user_state.cbs,
                       [WBXMLinfo]) of
             {Tag,{StrTbl,BinCodes}} ->
             US1=US#user_state{str_table=StrTbl},
             {{Tag,StrTbl,BinCodes},xmerl_scan:user_state(US1,S)};
             {error,Reason} ->
             throw({error,Reason})
         end
       end,
    Event=fun(#xmerl_event{data=Data}, S) ->
          case Data of
              Decl when record(Decl,xmlDecl) ->
              US=xmerl_scan:user_state(S),
              US1=US#user_state{xml=Decl},
              xmerl_scan:user_state(US1,S);
              _ ->
              S
          end
      end,

    User_state=#user_state{str_table=create_erl_stringtable([])},
    {USret,C2}=xmerl_scan:string(C1,
                 [{acc_fun, Acc},
                  {fetch_fun, Fetch},
                  {event_fun,Event},
                  {user_state,User_state},
                  {prolog,stop}
                 ]),
    ?debug("xmerl_scan with prolog=stop Charset:~p Mode:~p~n",[InCharset,Mode],encode),
    
    {OutCharset,C3}=
    case Mode of
        undefined ->
        case get_xml_encoding(USret#user_state.xml) of
            undefined -> % Character set undefined!
            guess_charset(C2);
            XMLcharset ->
            ?debug("XMLcharset=~p",[XMLcharset],encode),
            {XMLcharset,ucs:to_unicode(C2,XMLcharset)}
        end;
        auto ->
        case get_xml_encoding(USret#user_state.xml) of
            undefined -> % Assume Auto detected charset is correct
            {InCharset,C2};
            XMLcharset ->
            {XMLcharset,ucs:to_unicode(C2,XMLcharset)}
        end;
        external ->
        {InCharset,C2}
    end,
    ?debug("OutCharset: ~p Rest:~p~n",[OutCharset,C3],encode),
    
    USret2=USret#user_state{charset=OutCharset},
    Module=USret2#user_state.module,
    ModRules=
    case ets:lookup(DTDRules,Module) of
        [{_,MR}] ->
        MR;
        _ ->
        throw({error,dtd_not_supported})
    end,
    {Root,StrTbl2,WBXMLTokens}=
    xmerl_scan:string(C3,
              [{acc_fun, Acc},
               {hook_fun,Hook},
               {user_state,USret2},
               {rules,ModRules},
               {prolog,continue}
              ]),
    EncWBXMLVers=encode_wbxml_version(USret2#user_state.wbxml),
    EncPIcode=encode_public_identifier(Module),
    EncCharset=encode_charset(OutCharset),
    EncStrTbl=encode_stringtable(StrTbl2,OutCharset),
    remove_stringtable(User_state#user_state.str_table),
    {ok,
     EncWBXMLVers++EncPIcode++EncCharset++EncStrTbl++WBXMLTokens}.

%%% Some sites generate content without the XML Declaration (not "valid" XML)
get_xml_encoding(#xmlDecl{attributes=Attrlist}) ->
    lookup_attributelist(encoding,Attrlist);
get_xml_encoding(undefined) -> % No XML Declaration
    ?warning("No XML Declaration!",[],get_xml_encoding),
    undefined.

lookup_attributelist(Name,[]) ->
    undefined;
lookup_attributelist(Name,[#xmlAttribute{name=Name,value=Val}|Attrlist]) ->
    list_to_atom(string:to_lower(Val));
lookup_attributelist(Name,[H|Attrlist]) ->
    lookup_attributelist(Name,Attrlist).

%%% According to the XML standard the default character set is UTF-8.
%%% However, this have shown not be always true for actual WAP services.
%%% For now, assume it is UTF-8 encoded and if that fails try ISO-8859-1
guess_charset(C) ->
    ?debug("Assumes it is utf-8 encoded~n",[],encode),
    case ucs:to_unicode(C,'utf-8') of
    {error,Reason} ->
        ?warning("Mal-formed XML document! (not UTF-8) got ~p",
             [Reason],guess_charset), 
        case ucs:to_unicode(C,'iso-8859-1') of
        {error,Reason1} ->
            ?warning("... and not ISO-8859-1, got ~p",
                 [Reason1],guess_charset), 
            {error,Reason1};
        C2 ->
            {'iso-8859-1',C2}
        end;
    C1 ->
        {'utf-8',C1}
    end.


%%% ============================================================================
decode(Content) ->
    {Content1,WBXMLVers}=decode_wbxml_version(Content),
    BinWBXMLVers=get_bin(Content,Content1),
    %io:format("WBXMLVersion: ~p from ~p~n",[WBXMLVers,BinWBXMLVers]),
    {Content2,PIcode}=decode_public_identifier(Content1),
    BinPIcode=get_bin(Content1,Content2),
    %io:format("PICode: ~p from ~p~n",[PIcode,BinPIcode]),
    {Content3,Charset}=decode_charset(Content2),
    BinCharset=get_bin(Content2,Content3),
    %io:format("Charset: ~p from ~p~n",[Charset,BinCharset]),
    {Content4,StrTbl}=decode_stringtable(Content3),
    BinStrTbl=get_bin(Content3,Content4),
    %io:format("StringTable: ~p from ~p~n",[StrTbl,BinStrTbl]),
    Module=decode_cb_module(PIcode,StrTbl),
    %io:format("CallbackModule: ~w~n",[Module]),
    case catch decode_tokens(Content4,StrTbl,Charset,Module) of
    Body when list(Body) ->
        PI=decode_PI(PIcode,StrTbl),
        remove_stringtable(StrTbl),
        {ok,Body};
    Error ->
        %?error("ERROR decoding ~p, got ~p",[Content4,Error],decode),
        remove_stringtable(StrTbl),
        {error,cannot_decode}
    end.

%% FIXME! DTD identifer might be stored in the String Table
decode_cb_module(?WBXML_wml_10,_) -> wml_10;
decode_cb_module(?WBXML_wml_11,_) -> wml_11;
decode_cb_module(?WBXML_wml_12,_) -> wml_12;
decode_cb_module(?WBXML_wml_13,_) -> wml_13;
decode_cb_module(?WBXML_wta_event_10,_) -> wta_event_10;
decode_cb_module(?WBXML_si_10,_) -> si_10;
decode_cb_module(?WBXML_sl_10,_) -> sl_10;
decode_cb_module(?WBXML_co_10,_) -> co_10;
decode_cb_module(?WBXML_channel_11,_) -> channel_11;
decode_cb_module(?WBXML_channel_12,_) -> channel_12;
decode_cb_module(?WBXML_provisioning_10,_) -> provisioning_10;
decode_cb_module(?WBXML_wta_wml_12,_) -> wta_wml_12;
decode_cb_module(?WBXML_unknown,_) -> wml_11;
decode_cb_module(?WBXML_syncml_11,_) -> syncml_11;
decode_cb_module(?WBXML_devinf_11,_) -> devinf_11;
decode_cb_module(?WBXML_syncml_12,_) -> syncml_11;
decode_cb_module(?WBXML_devinf_12,_) -> devinf_11;
decode_cb_module(_,StrTbl) -> wml_11.


get_bin(Content,Content1) ->
    list_to_binary(lists:sublist(Content,length(Content)-length(Content1))).


%%% ----------------------------------------------------------------------------
decode_tokens(Content,StrTbl,Charset,Module) ->
    decode(Content,StrTbl,Charset,Module).


%%% ----------------------------------------------------------------------------
encode_wbxml_version(WBXMLVers) ->
    pack_uintvar(WBXMLVers).

decode_wbxml_version(Content) ->
    unpack_uintvar(Content).

%%% ----------------------------------------------------------------------------
%% Public Identifier code, or index in the string table.
encode_public_identifier(PI) ->
    [encode_PI(PI)].

decode_public_identifier([0|Content]) ->
    {C,Index}=unpack_uintvar(Content),
    {C,{index,Index}};
decode_public_identifier(Content) ->
    unpack_uintvar(Content).


%%% ----------------------------------------------------------------------------
encode_charset(Charset) ->
    case ucs:getMIB(Charset) of
    {error,undefined_charset} ->
        throw({error,undefined_charset});
    MIBnum ->
        pack_uintvar(MIBnum)
    end.

decode_charset(Content) ->
    {C1,MIBnum}=unpack_uintvar(Content),
    case ucs:getCharset(MIBnum) of
    {error,undefined_mibnum} ->
        throw({error,undefined_mibnum});
    Charset ->
        {C1,Charset}
    end.

%%% ----------------------------------------------------------------------------
encode_stringtable(StrTbl,Charset) ->
    EncStrTbl=create_wbxml_stringtable(StrTbl,Charset),
    pack_uintvar(length(EncStrTbl))++EncStrTbl.

decode_stringtable(Content) ->
    {C1,Len}=unpack_uintvar(Content),
    BinLen=get_bin(Content,C1),
    %io:format("StrTbl Len: ~w from ~p~n",[Len,BinLen]),
    {lists:nthtail(Len,C1),
     create_erl_stringtable(lists:sublist(C1,Len))}.

%%% ----------------------------------------------------------------------------
get_module("-//WAPFORUM//DTD WML 1.0//EN") -> {?WBXML_VERSION1,wml_10};
get_module("-//WAPFORUM//DTD WTA 1.1//EN") -> {?WBXML_VERSION1,wta_11};
get_module("-//WAPFORUM//DTD WML 1.1//EN") -> {?WBXML_VERSION1,wml_11};
get_module("-//WAPFORUM//DTD SI 1.0//EN") ->  {?WBXML_VERSION2,si_10};
get_module("-//WAPFORUM//DTD SL 1.0//EN") ->  {?WBXML_VERSION2,sl_10};
get_module("-//WAPFORUM//DTD CO 1.0//EN") ->  {?WBXML_VERSION2,co_10};
get_module("-//WAPFORUM//DTD CHANNEL 1.1//EN") -> {?WBXML_VERSION2,channel_11};
get_module("-//WAPFORUM//DTD WML 1.2//EN") ->     {?WBXML_VERSION2,wml_12};
get_module("-//WAPFORUM//DTD WML 1.3//EN") ->     {?WBXML_VERSION2,wml_13};
get_module("-//WAPFORUM//DTD PROV 1.0//EN") ->{?WBXML_VERSION2,provisioning_10};
get_module("-//WAPFORUM//DTD WTA-WML 1.2//EN") -> {?WBXML_VERSION2,wta_wml_12};
get_module("-//WAPFORUM//DTD CHANNEL 1.2//EN") -> {?WBXML_VERSION2,channel_12};
get_module("-//SYNCML//DTD SyncML 1.1//EN") -> {?WBXML_VERSION2,syncml_11};
get_module("-//SYNCML//DTD DevInf 1.1//EN") -> {?WBXML_VERSION2,devinf_11};
get_module("-//SYNCML//DTD SyncML 1.2//EN") -> {?WBXML_VERSION2,syncml_12};
get_module("-//SYNCML//DTD DevInf 1.2//EN") -> {?WBXML_VERSION2,devinf_12};
get_module(PI) ->
    throw({error,unknown_pi}).

%%% SGML Public Identifier
encode_PI(wml_10) -> ?WBXML_wml_10;
encode_PI(wta_event_10) -> ?WBXML_wta_event_10;
encode_PI(wml_11) -> ?WBXML_wml_11;
encode_PI(si_10) -> ?WBXML_si_10;
encode_PI(sl_10) -> ?WBXML_sl_10;
encode_PI(co_10) -> ?WBXML_co_10;
encode_PI(channel_11) -> ?WBXML_channel_11;
encode_PI(wml_12) -> ?WBXML_wml_12;
encode_PI(wml_13) -> ?WBXML_wml_13;
encode_PI(provisioning_10) -> ?WBXML_provisioning_10;
encode_PI(wta_wml_12) -> ?WBXML_wta_wml_12;
encode_PI(channel_12) -> ?WBXML_channel_12;
encode_PI(syncml_11) -> ?WBXML_syncml_11;
encode_PI(devinf_11) -> ?WBXML_devinf_11;
encode_PI(syncml_12) -> ?WBXML_syncml_12;
encode_PI(devinf_12) -> ?WBXML_devinf_12;
encode_PI(A) ->
    A.



decode_PI({index,Index},StrTab) ->
    lookup_index(StrTab,Index);
decode_PI(?WBXML_unknown,_) ->
    "";
decode_PI(?WBXML_wml_10,_) ->
    "<!DOCTYPE wml PUBLIC \"-//WAPFORUM//DTD WML 1.0//EN\" \"http://www.wapforum.org/DTD/wml_1.0.xml\">";
decode_PI(?WBXML_wta_event_10,_) ->
    "<!DOCTYPE wta PUBLIC \"-//WAPFORUM//DTD WTA 1.1//EN\" \"http://www.wapforum.org/DTD/wta_1.0.xml\">";
decode_PI(?WBXML_wml_11,_) ->
    "<!DOCTYPE wml PUBLIC \"-//WAPFORUM//DTD WML 1.1//EN\" \"http://www.wapforum.org/DTD/wml_1.1.xml\">";
decode_PI(?WBXML_si_10,_) ->
    "<!DOCTYPE si PUBLIC \"-//WAPFORUM//DTD SI 1.0//EN\" \"http://www.wapforum.org/DTD/si_1.0.xml\">";
decode_PI(?WBXML_sl_10,_) ->
    "<!DOCTYPE sl PUBLIC \"-//WAPFORUM//DTD SL 1.0//EN\" \"http://www.wapforum.org/DTD/sl_1.0.xml\">";
decode_PI(?WBXML_co_10,_) ->
    "<!DOCTYPE co PUBLIC \"-//WAPFORUM//DTD CO 1.0//EN\" \"http://www.wapforum.org/DTD/co_1.0.xml\">";
decode_PI(?WBXML_channel_11,_) ->
    "<!DOCTYPE channel PUBLIC \"-//WAPFORUM//DTD CHANNEL 1.1//EN\" \"http://www.wapforum.org/DTD/channel_1.1.xml\">";
decode_PI(?WBXML_wml_12,_) ->
    "<!DOCTYPE wml PUBLIC \"-//WAPFORUM//DTD WML 1.2//EN\" \"http://www.wapforum.org/DTD/wml_1.2.xml\">";
decode_PI(?WBXML_wml_13,_) ->
    "<!DOCTYPE wml PUBLIC \"-//WAPFORUM//DTD WML 1.3//EN\" \"http://www.wapforum.org/DTD/wml_1.3.xml\">";
decode_PI(?WBXML_provisioning_10,_) ->
    "<!DOCTYPE prov PUBLIC \"-//WAPFORUM//DTD PROV 1.0//EN\" \"http://www.wapforum.org/DTD/prov_1.0.xml\">";
decode_PI(?WBXML_wta_wml_12,_) ->
    "<!DOCTYPE wta-wml PUBLIC \"-//WAPFORUM//DTD WTA-WML 1.2//EN\" \"http://www.wapforum.org/DTD/wta-wml_1.2.xml\">";
decode_PI(?WBXML_channel_12,_) ->
    "<!DOCTYPE channel PUBLIC \"-//WAPFORUM//DTD CHANNEL 1.2//EN\" \"http://www.wapforum.org/DTD/channel_1.2.xml\">";
decode_PI(?WBXML_syncml_11,_) ->
    "<!DOCTYPE syncml PUBLIC \"-//SYNCML//DTD SyncML 1.1//EN\" \"http://www.syncml.org/docs/syncml_represent_v11_20020213.dtd\">";
decode_PI(?WBXML_devinf_11,_) ->
    "<!DOCTYPE devinf PUBLIC \"-//SYNCML//DTD DevInf 1.1//EN\" \"http://www.syncml.org/docs/devinf_v11_20020215.dtd\">";
decode_PI(?WBXML_syncml_12,_) ->
    "<!DOCTYPE syncml PUBLIC \"-//SYNCML//DTD SyncML 1.2//EN\" \"http://www.syncml.org/docs/syncml_represent_v11_20020213.dtd\">";
decode_PI(?WBXML_devinf_12,_) ->
    "<!DOCTYPE devinf PUBLIC \"-//SYNCML//DTD DevInf 1.2//EN\" \"http://www.syncml.org/docs/devinf_v11_20020215.dtd\">".


%%% ============================================================================
%%% String table handling
%% Note:
%% - Currently implemented as simple key/value list which is probably faster
%%   than using ets, because of the setup times and usually rather small tables
%%   anyway.
%% - String tables are created empty when encoding, but directly filled with
%%   content when decoding.
%% - StrTab is kept sorted!

create_erl_stringtable([]) ->
    [];
create_erl_stringtable(Data) ->
    create_erl_stringtable([],Data,0).

create_erl_stringtable(StrTab,[],_) ->
    StrTab;
create_erl_stringtable(StrTab,Data,Index) ->
    {Data1,Str}=wsp_bytecodes:decode_string(Data),
    create_erl_stringtable([{Index,Str}|StrTab],Data1,Index+length(Str)+1).

create_wbxml_stringtable([],_) ->
    [];
create_wbxml_stringtable(StrTab,Charset) ->
    create_wbxml_stringtable2(lists:reverse(StrTab),Charset).

create_wbxml_stringtable2([],_) ->
    [];
create_wbxml_stringtable2([{_,Str}|Rest],Charset) ->
    Str2=ucs:from_unicode(Str,Charset),
    binary_to_list(wsp_bytecodes:encode_string(Str2))++
    create_wbxml_stringtable2(Rest,Charset).

remove_stringtable(StrTbl) ->
    [].
% ets:delete(StrTbl).


add_string([],Val) ->
    {[{0,Val}],0};
add_string(StrTbl,Val) ->
    case lists:keysearch(Val,2,StrTbl) of
    {value,{Index,Val}} ->
        {StrTbl,Index};
    _ ->
        {I1,TblVal}=hd(StrTbl),
        Index=I1+length(TblVal)+1,
        {[{Index,Val}|StrTbl],Index}
    end.


lookup_index(StrTab,Index) ->
    case lists:keysearch(Index,1,StrTab) of
    {value,{_,Val}} ->
        Val;
    _ ->
        ?error("Index ~p not found in string table",
           [Index],lookup_index),
        throw({error,not_found})
    end.

%    case ets:lookup(StrTab,Key) of
%   [{Key,Val}]  ->
%       Val;
%   [] ->
%       ?error("Key ~p not found in string table",[Key],lookup_stringtable),
%       throw({error,not_found})
%    end.


%%%%%%%%%%%%%%%%%%%%%% DECODE

decode(Content,StrTbl,Charset,Module) ->
    tag_mode(Module,Content,StrTbl,Charset,[],1,[]).

%% TAG MODE
tag_mode(_,[],_,_,_,_,Result) ->
    Result;
tag_mode(Module,[?WBXML_END|Content],StrTbl,Charset,[Tag|Tagstack],TagPage,Result) ->
    %io:format("</~s> tail: ~P~n",[Tag,Content,10]),
    tag_mode(Module,Content,StrTbl,Charset,Tagstack,TagPage,Result++"</"++Tag++">");
tag_mode(Module,[?WBXML_LITERAL_AC|Content],StrTbl,Charset,Tagstack,TagPage,Result) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Tag=wbxml:lookup_index(StrTbl,Index),
    {Content3,Attributes}=attribute_mode(Module,Content2,StrTbl,Charset,true,[]),
    tag_mode(Module,Content3,StrTbl,Charset,[Tag|Tagstack],TagPage,Result++"<"++Tag++" "++Attributes++">");
tag_mode(Module,[?WBXML_LITERAL_A|Content],StrTbl,Charset,Tagstack,TagPage,Result) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Tag=wbxml:lookup_index(StrTbl,Index),
    {Content3,Attributes}=attribute_mode(Module,Content,StrTbl,Charset,true,[]),
    tag_mode(Module,Content3,StrTbl,Charset,Tagstack,TagPage,Result++"<"++Tag++" "++Attributes++"/>");
tag_mode(Module,[?WBXML_LITERAL_C|Content],StrTbl,Charset,Tagstack,TagPage,Result) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Tag=wbxml:lookup_index(StrTbl,Index),
    tag_mode(Module,Content2,StrTbl,Charset,[Tag|Tagstack],TagPage,Result++"<"++Tag++">");
tag_mode(Module,[?WBXML_LITERAL|Content],StrTbl,Charset,Tagstack,TagPage,Result) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Tag=wbxml:lookup_index(StrTbl,Index),
    tag_mode(Module,Content2,StrTbl,Charset,Tagstack,TagPage,Result++"<"++Tag++"/>");
tag_mode(Module,[Code|Content],StrTbl,Charset,Tagstack,TagPage,Result)
    when 16#00=<Code,Code=<16#04;16#40=<Code,Code=<16#44;
         16#80=<Code,Code=<16#84;16#c0=<Code,Code=<16#c4 ->
    {Content2,Str}=global_wbxml_code(Code,Content,StrTbl,Charset),
    case Code of
    ?WBXML_SWITCH_PAGE -> 
        TagPageSelect = Str + 1,
        %io:format("~nSWITCH_PAGE: ~p~n",[Str]),
        tag_mode(Module,Content2,StrTbl,Charset,Tagstack,TagPageSelect,Result);
    ?WBXML_OPAQUE ->
        %io:format("~s",[Str]),
        tag_mode(Module,Content2,StrTbl,Charset,Tagstack,TagPage,Result++Str);
    _ -> 
        tag_mode(Module,Content2,StrTbl,Charset,Tagstack,TagPage,Result++Str)
    end;
tag_mode(Module,[Code|Content],StrTbl,Charset,Tagstack,TagPage,Result) when Code>=192 ->
    Tag=apply(Module,lookup_tag,[Code band 63,TagPage]),
    {Content2,Attributes}=attribute_mode(Module,Content,StrTbl,Charset,true,[]),
    tag_mode(Module,Content2,StrTbl,Charset,[Tag|Tagstack],TagPage,Result++"<"++Tag++" "++Attributes++">");
tag_mode(Module,[Code|Content],StrTbl,Charset,Tagstack,TagPage,Result) when Code>=128->
    Tag=apply(Module,lookup_tag,[Code band 63,TagPage]),
    {Content2,Attributes}=attribute_mode(Module,Content,StrTbl,Charset,true,[]),
    tag_mode(Module,Content2,StrTbl,Charset,Tagstack,TagPage,Result++"<"++Tag++" "++Attributes++"/>");
tag_mode(Module,[Code|Content],StrTbl,Charset,Tagstack,TagPage,Result) when Code>=64->
    Tag=apply(Module,lookup_tag,[Code band 63,TagPage]),
    %io:format("<~s>",[Tag]),
    tag_mode(Module,Content,StrTbl,Charset,[Tag|Tagstack],TagPage,Result++"<"++Tag++">");
tag_mode(Module,[Code|Content],StrTbl,Charset,Tagstack,TagPage,Result) ->
    Tag=apply(Module,lookup_tag,[Code,TagPage]),
    %io:format("<~s/>~n",[Tag]),
    tag_mode(Module,Content,StrTbl,Charset,Tagstack,TagPage,Result++"<"++Tag++"/>").

% ATTRIBUTE MODE
attribute_mode(Module,[?WBXML_END|Content],_,_,_,Result) ->
    %io:format("gcode: END (of attribute) tail ~P~n",[Content,10]),
    {Content,Result++"\""};
attribute_mode(Module,[Code|Content],StrTbl,Charset,First,Result)
  when 16#00=<Code,Code=<16#04; 16#40=<Code,Code=<16#44;
       16#80=<Code,Code=<16#84; 16#c0=<Code,Code=<16#c4 ->
    {Content2,Str}=global_wbxml_code(Code,Content,StrTbl,Charset),
    %io:format("gcode: ~p with ~p tail ~P~n",[Code,Str,Content2,10]),
    attribute_mode(Module,Content2,StrTbl,Charset,First,Result++Str);
attribute_mode(Module,[Code|Content],StrTbl,Charset,First,Result) ->
    {Attribute,First2}=apply(Module,lookup_attribute,[Code,First]),
    %io:format("attribute: ~p from ~p tail ~P~n",[Attribute,Code,Content,10]),
    attribute_mode(Module,Content,StrTbl,Charset,First2,Result++Attribute);
attribute_mode(_,[],_,_,_,Result) ->
    {error,premature_end}.

global_wbxml_code(?WBXML_ENTITY,Content,StrTbl,Charset) ->
    {Content2,Entity}=wbxml:unpack_uintvar(Content),
    {Content2,lookup_entity(Entity,Charset)};
global_wbxml_code(?WBXML_STR_I,Content,StrTbl,_) ->
    wsp_bytecodes:decode_string(Content);
global_wbxml_code(?WBXML_STR_T,Content,StrTbl,_) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    {Content2,wbxml:lookup_index(StrTbl,Index)};
global_wbxml_code(?WBXML_EXT_I_0,Content,StrTbl,_) ->
    {Content2,Str}=wsp_bytecodes:decode_string(Content),
    {Content2,"$$("++Str++":escape)"};
global_wbxml_code(?WBXML_EXT_I_1,Content,StrTbl,_) ->
    {Content2,Str}=wsp_bytecodes:decode_string(Content),
    {Content2,"$$("++Str++":unesc)"};
global_wbxml_code(?WBXML_EXT_I_2,Content,StrTbl,_) ->
    {Content2,Str}=wsp_bytecodes:decode_string(Content),
    {Content2,"$$("++Str++")"};
global_wbxml_code(?WBXML_EXT_T_0,Content,StrTbl,_) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Str=wbxml:lookup_index(StrTbl,Index),
    {Content2,"$$("++Str++":escape)"};
global_wbxml_code(?WBXML_EXT_T_1,Content,StrTbl,_) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Str=wbxml:lookup_index(StrTbl,Index),
    {Content2,"$$("++Str++":unesc)"};
global_wbxml_code(?WBXML_EXT_T_2,Content,StrTbl,_) ->
    {Content2,Index}=wbxml:unpack_uintvar(Content),
    Str=wbxml:lookup_index(StrTbl,Index),
    {Content2,"$$("++Str++")"};
global_wbxml_code(?WBXML_SWITCH_PAGE,Content,StrTbl,_) ->
    [Page|Tail]=Content,
    {Tail,Page};
global_wbxml_code(?WBXML_PI,Content,StrTbl,_) ->
    {error,cannot_read_pi};
global_wbxml_code(?WBXML_OPAQUE,Content,StrTbl,_) ->
    {TailContent,OpaqueLength}=wbxml:unpack_uintvar(Content),
    Str = lists:sublist(TailContent,1,OpaqueLength),
    Tail = lists:sublist(TailContent,OpaqueLength+1,length(TailContent)),
    {Tail,Str};
global_wbxml_code(Code,Content,StrTbl,_) ->
    %io:format("Code ~p Tail ~P~n",[Code,Content,10]),
    [Code|Content2]=Content,
    {Content2,Code}.
    
lookup_entity(Code,Charset) ->
    case lists:keysearch(Code,1,?WBXML_ENTITIES) of
    {value,{Code,Val}} ->
        Val;
    _ ->
        if 
        Code<128 -> Code;
        true -> ucs:from_unicode([Code],Charset)
        end
    end.

