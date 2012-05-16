%%% File    : wsp_bytecodes.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : WSP HTTP and WAP header encoding
%%% Created : 14 Oct 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_bytecodes).
-author('johblo@dragon.cellpt.se').

-include("wsp.hrl").

-revision('$Revision: 1.1.1.1 $\n').
-rcsid('@(#) $Id: wsp_bytecodes.erl,v 1.1.1.1 2001/07/04 14:51:13 uid56739 Exp $\n').

-export([encode_status/1,decode_status/1,
	 encode_capabils/2,decode_capabils/3,
	 encode_data/3,decode_data/2,
	 encode_version_data/1,decode_version_data/1,
	 encode_string/1,decode_string/1,
	 pp_cap/1,
	 bool_to_bin/1,bin_to_bool/1
	]).

-import(wap_common,[pack_uintvar/1,unpack_uintvar/1,
		  pack_uint8/1,unpack_uint8/1]).

-define(DBG(X), X).


%% =============================================================================
%% Well-known Status Code Assignments
%% Encodes HTTP status codes to Assigned numbers in WAP
%% Other Status codes are left untouched.
encode_status(Status) when Status>=100,Status=<101 ->
    (Status div 100) * 16#10 + (Status rem 100);
encode_status(Status) when Status>=200,Status=<206 ->
    (Status div 100) * 16#10 + (Status rem 100);
encode_status(Status) when Status>=300,Status=<305 ->
    (Status div 100) * 16#10 + (Status rem 100);
encode_status(Status) when Status>=400,Status=<415 ->
    (Status div 100) * 16#10 + (Status rem 100);
encode_status(Status) when Status>=500,Status=<505 ->
    (Status div 100) * 16#10 + (Status rem 100) + 16;
encode_status(Status) ->
    throw({error,unknown_status}).

%% Decodes Assigned numbers in WAP to HTTP status codes
decode_status(ANum) when ANum>=16#10,ANum=<16#11 ->
    (ANum div 16#10) * 100 + (ANum rem 16#10);
decode_status(ANum) when ANum>=16#20,ANum=<16#26 ->
    (ANum div 16#10) * 100 + (ANum rem 16#10);
decode_status(ANum) when ANum>=16#30,ANum=<16#35 ->
    (ANum div 16#10) * 100 + (ANum rem 16#10);
decode_status(ANum) when ANum>=16#40,ANum=<16#4f ->
    (ANum div 16#10) * 100 + (ANum rem 16#10);
decode_status(ANum1) when ANum1>=16#60,ANum1=<16#65 ->
    ANum=ANum1-16,
    (ANum div 16#10) * 100 + (ANum rem 16#10);
decode_status(ANum) ->
    ANum.

%%% ============================================================================
%%% Encode Capabilities
%% Note:
%% - Capabilities are only encoded if differing from default values.
encode_capabils(Cap,DefCap) ->
    BinClientSDU=
	if
	    Cap#cap.client_sdu==DefCap#cap.client_sdu ->
		<<>>;
	    true ->
		BinCSDU=wsp_pdu:encode_uintvar(Cap#cap.client_sdu),
		<<(wsp_pdu:decode_uintvar(size(BinCSDU)+1))/binary,
		1:1,?Client_SDU_Size:7,BinCSDU/binary>>
	end,
    BinServerSDU=
	if
	    Cap#cap.server_sdu==DefCap#cap.server_sdu ->
		<<>>;
	    true ->
		BinSSDU=wsp_pdu:encode_uintvar(Cap#cap.server_sdu),
		<<(wsp_pdu:decode_uintvar(size(BinSSDU)+1))/binary,
		1:1,?Server_SDU_Size:7,BinSSDU/binary>>
	end,
    BinProtocolOptions=
	if
	    Cap#cap.cpushf==DefCap#cap.cpushf,
	    Cap#cap.pushf==DefCap#cap.pushf,
	    Cap#cap.sresumef==DefCap#cap.sresumef,
	    Cap#cap.ackhead==DefCap#cap.ackhead ->
		<<>>;
	    true ->
		<<(wsp_pdu:decode_uintvar(1))/binary,1:1,?ProtocolOptions:7,
		(bool_to_bin(Cap#cap.cpushf)):1,
		(bool_to_bin(Cap#cap.pushf)):1,
		(bool_to_bin(Cap#cap.sresumef)):1,
		(bool_to_bin(Cap#cap.ackhead)):1,
		0:4>>
	end,
    BinMOM=
	if
	    Cap#cap.mom==DefCap#cap.mom ->
		<<>>;
	    true ->
		<<(wsp_pdu:decode_uintvar(2))/binary,
		1:1,?Method_MOR:7, (Cap#cap.mom):8>>
	end,
    BinMOP=
	if
	    Cap#cap.mop==DefCap#cap.mop ->
		<<>>;
	    true ->
		<<(wsp_pdu:decode_uintvar(2))/binary,
		1:1,?Push_MOR:7,(Cap#cap.mop):8>>
	end,
    BinExtendedMethods=
	if
	    Cap#cap.extended_methods==DefCap#cap.extended_methods ->
		<<>>;
	    true ->
		BinEM=encode_em_val(Cap#cap.extended_methods),
		<<(wsp_pdu:decode_uintvar(size(BinEM)+1))/binary,
		1:1,?ExtendedMethods:7,BinEM/binary>>
	end,
    BinHeaderCodePages=
	if
	    Cap#cap.header_code_pages==DefCap#cap.header_code_pages ->
		<<>>;
	    true ->
		BinHCP=encode_hcp_val(Cap#cap.header_code_pages),
		<<(wsp_pdu:decode_uintvar(size(BinHCP)+1))/binary,
		1:1,?HeaderCodePages:7,BinHCP/binary>>
	end,
    BinAliases=
	if
	    Cap#cap.aliases==DefCap#cap.aliases ->
		<<>>;
	    true ->
		BinA=encode_aliases_val(Cap#cap.aliases),
		<<(wsp_pdu:decode_uintvar(size(BinA)+1))/binary,
		1:1,?Aliases:7,BinA/binary>>
	end,
    BinOther=
	if
	    Cap#cap.other==DefCap#cap.other ->
		<<>>;
	    true ->
		encode_other(Cap#cap.other)
	end,
    concat_binary([BinClientSDU,BinServerSDU,BinProtocolOptions,
		   BinMOM,BinMOP,BinExtendedMethods,BinHeaderCodePages,
		   BinAliases,BinOther]).


encode_em_val([]) ->
    <<>>;
encode_em_val([{Name,Type}|List]) ->
    <<1:1,Type:7,(encode_string(Name))/binary,(encode_em_val(List))/binary>>.

encode_hcp_val([]) ->
    <<>>;
encode_hcp_val([{Name,Code}|List]) ->
    <<1:1,Code:7,(encode_string(Name))/binary,(encode_hcp_val(List))/binary>>.

encode_aliases_val(List) ->
    wsp_pdu:encode_addresslist(List).
	
encode_other([]) ->
    <<>>;
encode_other([{Id,Param}|List]) ->
    BinStr=encode_string(Id),
    <<(wsp_pdu:encode_uintvar(size(BinStr)))/binary,BinStr/binary,
    (encode_other(List))/binary>>.
    
    
%% -----------------------------------------------------------------------------
%%% Decodes Capabilites
%% Note:
%% - Overrides all previous capability definitions
%% - Encoded capabilites are coded with Short-integer encoding, ie with
%%   highest priority bit set.
%% - If multiple capabilities of the same type are received always pick the
%%   latest (the one parsed last) 
%% - Collect all unknown capabilities in the other field
decode_capabils(Input,0,DefCap) ->
    {Input,DefCap};
decode_capabils(Input,Len,DefCap) ->
    {BinCapabils,Content}=split_binary(Input,Len),
    {Content,decode_capabils2(BinCapabils,DefCap)}.

decode_capabils2(<<>>,Cap) ->
    Cap;
decode_capabils2(Input,Cap) ->
    {Input1,Len}=wsp_pdu:decode_uintvar(Input),
    {Input2,NewCap}=decode_cap(Input1,Len,Cap),
    decode_capabils2(Input2,NewCap).
    
decode_cap(<<1:1,?Client_SDU_Size:7,Content/binary>>,Len,Cap) ->
    {BinCap,Content2}=split_binary(Content,Len-1),
    {<<>>,Size}=wsp_pdu:decode_uintvar(BinCap),
    {Content2,Cap#cap{client_sdu=Size}};
decode_cap(<<1:1,?Server_SDU_Size:7,Content/binary>>,Len,Cap) ->
    {BinCap,Content2}=split_binary(Content,Len-1),
    {<<>>,Size}=wsp_pdu:decode_uintvar(BinCap),
    {Content2,Cap#cap{server_sdu=Size}};
decode_cap(<<1:1,?ProtocolOptions:7,
	   ConfPushF:1,PushF:1,SessionResumeF:1,AckHeaderF:1,0:4,
	   Content/binary>>,2,Cap) ->
    {Content,Cap#cap{cpushf=bin_to_bool(ConfPushF),
		     pushf=bin_to_bool(PushF),
		     sresumef=bin_to_bool(SessionResumeF),
		     ackhead=bin_to_bool(AckHeaderF)}};
decode_cap(<<1:1,?Method_MOR:7,MOR:8,Content/binary>>,2,Cap) ->
    {Content,Cap#cap{mom=MOR}};
decode_cap(<<1:1,?Push_MOR:7,MOR:8,Content/binary>>,2,Cap) ->
    {Content,Cap#cap{mop=MOR}};
decode_cap(<<1:1,?ExtendedMethods:7,Content/binary>>,Len,Cap) ->
    {BinCap,Content2}=split_binary(Content,Len-1),
    {Content2,Cap#cap{extended_methods=decode_em_val(binary_to_list(BinCap))}};
decode_cap(<<1:1,?HeaderCodePages:7,Content/binary>>,Len,Cap) ->
    {BinCap,Content2}=split_binary(Content,Len-1),
    {Content2,
     Cap#cap{header_code_pages=decode_hcp_val(binary_to_list(BinCap))}};
decode_cap(<<1:1,?Aliases:7,Content/binary>>,Len,Cap) ->
    {BinCap,Content2}=split_binary(Content,Len-1),
    {Content2,Cap#cap{aliases=decode_aliases_val(binary_to_list(BinCap))}};
decode_cap(<<Content/binary>>,Len,Cap) -> % Unknown Capability
    {BinCap,Content2}=split_binary(Content,Len),
    {Val,Identifier}=decode_string(binary_to_list(BinCap)),
    ?warning("Unknown capability skipping... ~p",[{Identifier,Val}],decode_cap),
    {Content2,Cap#cap{other=Cap#cap.other++[{Identifier,Val}]}}.

decode_em_val([]) ->
    [];
decode_em_val([Code|Input]) when 16#50=<Code,Code=<16#5f ->
    {Input2,Name}=decode_string(Input),    
    [{list_to_atom(Name),Code,get}|decode_em_val(Input2)];
decode_em_val([Code|Input]) when 16#70=<Code,Code=<16#7f ->
    {Input2,Name}=decode_string(Input),
    [{list_to_atom(Name),Code,post}|decode_em_val(Input2)].


decode_hcp_val([]) ->
    [];
decode_hcp_val([Code|Input]) ->
    {Input2,Name}=decode_string(Input),
    [{list_to_atom(Name),Code}|decode_hcp_val(Input2)].

decode_aliases_val(Input) ->
    wsp_pdu:decode_addresslist(Input).


pp_cap(#cap{client_sdu=Csdu,     % (uintvar) Max size client can receive
	    server_sdu=Ssdu,     % (uintvar) Max size server can receive
	    cpushf=CPushF,        % (bool) Support of Confirmed Push
	    pushf=PushF,         % (bool) Support of Push
	    sresumef=SResumeF,      % (bool) Support of Session Resume
	    ackhead=AckHead,       % (bool) Support of Ack Headers
	    mom=MOM,               % (uint8) Max outstanding methods
	    mop=MOP,               % (uint8) Max outstanding pushes
	    extended_methods=EM, % (list) 
	    header_code_pages=HCP,% (list)
	    aliases=Aliases,          % (list)
	    other=Other}) ->
    io:format("  Cap client_sdu ~p~n",[Csdu]),
    io:format("  Cap server_sdu ~p~n",[Ssdu]),
    io:format("  Cap cpushf ~p~n",[CPushF]),
    io:format("  Cap pushf ~p~n",[PushF]),
    io:format("  Cap sresumef ~p~n",[SResumeF]),
    io:format("  Cap ackhead ~p~n",[AckHead]),
    io:format("  Cap mom ~p~n",[MOM]),
    io:format("  Cap mop ~p~n",[MOP]),
    io:format("  Cap extended_methods ~p~n",[EM]),
    io:format("  Cap header_code_pages ~p~n",[HCP]),
    io:format("  Cap aliases ~p~n",[Aliases]),
    io:format("  Cap other ~p~n",[Other]).
    

%% .............................................................................
%% Converts 1/0 to true/false
bin_to_bool(?TRUE) -> true;
bin_to_bool(?FALSE) -> false.

bool_to_bin(true) -> ?TRUE;
bool_to_bin(false) -> ?FALSE.


%% .............................................................................
%% Adds an extra 0
encode_string(Str) when atom(Str) ->
    list_to_binary(atom_to_list(Str) ++ [0]);
encode_string(Str) ->
    list_to_binary(Str ++ [0]).

%% Extracts a string, in particular empty strings are possible
%% Removes extra 0 and returns a tuple {Rest,String}
decode_string([0]) ->      {[],[]};
decode_string([0|Rest]) -> {Rest,[]};
decode_string(C) ->    
    case string:chr(C,0) of
	0 ->    throw({error,illegal_textstring});
	X ->    {lists:nthtail(X,C),lists:sublist(C,1,X-1)}
    end.


%% =============================================================================
%% ContentType = 'application/vnd.wap.multipart.mixed' etc
encode_data(CTCode,Data,Environ) ->
    case well_known_code(CTCode,?WSP_WELL_KNOWN_MULTIPART_CONTENT_TYPES) of
	true ->
	    MultiLen=wsp_pdu:decode_uintvar(length(Data)),
	    MultiLen ++ encode_multipart(Data,Environ,[]);
	_ ->
	    Data
    end.

encode_multipart([],_,Output) -> Output;
encode_multipart([{ContType,Hd,Data}|Rest],{E,C},Output) ->
    Ect=wsp_bytescodes_headers:encode_content_type(ContType,E),
    Headers=wsp_bytescodes_headers:encode_headers(Hd,{E,C}),
    HeadersLen=pack_uintvar(length(Headers)+length(Ect)),
    BData=encode_data(Ect,Data,{E,C}),
    DataLen=pack_uintvar(length(BData)),
    Entry=HeadersLen ++ DataLen ++ Ect ++ Headers ++ BData,
    encode_multipart(Rest,{E,C},Output ++ Entry).

%% -----------------------------------------------------------------------------
decode_data(CT,Content,ContentLen) ->
    case well_known_field(CT,?WSP_WELL_KNOWN_MULTIPART_CONTENT_TYPES) of
	true ->
	    Data=string:sub_string(Content,1,ContentLen),
	    {Data1,MultiLen}=unpack_uintvar(Data),
	    Multipart=decode_multipart(Data1,MultiLen),
	    Content1=lists:nthtail(ContentLen,Content),
	    {Content1,Multipart};
	_ ->
	    Data=string:sub_string(Content,1,ContentLen),
	    Content1=lists:nthtail(ContentLen,Content),
	    {Content1,Data}
    end.

%% .............................................................................
decode_data('application/vnd.wap.multipart.*',Content) ->
    decode_mpdata(Content);
decode_data('application/vnd.wap.multipart.mixed',Content) ->
    decode_mpdata(Content);
decode_data('application/vnd.wap.multipart.form-data',Content) ->
    decode_mpdata(Content);
decode_data('application/vnd.wap.multipart.byteranges',Content) ->
    decode_mpdata(Content);
decode_data('application/vnd.wap.multipart.alternative',Content) ->
    decode_mpdata(Content);
decode_data(_,Data) ->
    {[],Data}.

decode_mpdata(Content) ->
    {Content1,MultiLen}=unpack_uintvar(Content),
    {[],lists:reverse(decode_multipart(Content1,MultiLen))}.

%% .............................................................................
decode_multipart(Content1,0) ->
    [];
decode_multipart(Content1,MultiLen) ->
    {Content2,HeadersLen}=unpack_uintvar(Content1),
    {[Code|Content3],DataLen}=unpack_uintvar(Content2),
    {Content4,ContentType}=
	if
	    Code>128 ->
		{ok,CT}=wsp_bytecodes_headers:decode_content_type(Code),
		{Content3,CT};
	    true ->
		decode_string([Code|Content3])
	end,
    CtLen=length(Content3)-length(Content4),
    {Content5,Headers}=wsp_bytescodes_headers:decode_headers(Content4,
							     HeadersLen-CtLen),
    {Content6,Data}=decode_data(ContentType,Content5,DataLen),
    [{ContentType,Headers,Data} | decode_multipart(Content6,MultiLen-1)].

%% =============================================================================
%% Version number for  HTTP header and WSP Connect PDU version encoding
encode_version_data({Major,Minor}) ->
    ((Major bsl 4) band 240) bor (Minor band 15);
encode_version_data(_) ->
    throw({error,illegal_version}).

decode_version_data(Byte) ->
    Minor=Byte band 15,
    Major=((Byte bsr 4) band 15),
    {Major,Minor}.

%% .............................................................................
well_known_field(Val,[{_,_,Vallist}|Rest]) ->
    case lists:member(Val,Vallist) of
	true ->
	    true;
	_ ->
	    well_known_field(Val,Rest)
    end;
well_known_field(_,[]) ->
    false.

well_known_code(Code,[{Code,_,_}|Rest]) ->
    true;
well_known_code(Code,[_|Rest]) ->
    well_known_code(Code,Rest);
well_known_code(_,[]) ->
    false.

