%%% File    : wsp_pdu.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Bit coding/decoding of WSP PDUs
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wsp_pdu).
-author('johblo@dragon.cellpt.se').
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wsp_pdu.erl,v 1.1.1.1 2001/07/04 14:51:11 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:11 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([
	 encode/2,decode/2,pp_spdu/1,
	 encode_uintvar/1,decode_uintvar/1,
	 encode_addresslist/2,decode_addresslist/2,
	 decode_abort_reason/1 % Used by wtp_pdu
	]).

-include("wsp.hrl").

%%% ============================================================================
%%% Encodes A WSP PDU record into a binary Message PDU
%% Note:
%% - Does not distinguish between WSP CL and CO, thus the Tid field in a WSP CL
%%   PDU is added elsewhere.
encode(#connect{version={Major_vers,Minor_vers},capabilities=Cap,
		headers=Headers},Env) ->
    BinCap=wsp_bytecodes:encode_capabils(Cap,#cap{}),
    CapLen=encode_uintvar(size(BinCap)),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    HeaderLen=encode_uintvar(size(BinHeaders)),
    <<?Connect:8,Major_vers:4,Minor_vers:4,CapLen/binary,HeaderLen/binary,
    BinCap/binary,BinHeaders/binary>>;
encode(#connect_reply{server_sessionid=SessionId,capabilities=Cap,
		      headers=Headers},Env) ->
    BinSessionId=encode_uintvar(SessionId),
    BinCap=wsp_bytecodes:encode_capabils(Cap,#cap{}),
    CapLen=encode_uintvar(size(BinCap)),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    HeaderLen=encode_uintvar(size(BinHeaders)),
    <<?ConnectReply:8,BinSessionId/binary,CapLen/binary,HeaderLen/binary,
    BinCap/binary,BinHeaders/binary>>;
encode(#redirect{permanent=Permanent,
		 reuse_secure_session=ReuseSecSession,
		 rediradresses=AddressList},Env) ->
    <<?Redirect:8,Permanent:1,ReuseSecSession:1,0:6,
    (encode_addresslist(AddressList,Env#env.destination))/binary>>;
encode(#disconnect{server_sessionid=SessionId},Env) ->
    BinSessionId=encode_uintvar(SessionId),
    <<?Disconnect:8,BinSessionId/binary>>;
encode(#get{type=Method,uri=URI,headers=Headers},Env) ->
    Type=encode_method_type(Method,get,(Env#env.cap)#cap.extended_methods),
    BinURI=list_to_binary(URI),
    URILen=encode_uintvar(size(BinURI)),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    <<Type:8,URILen/binary,BinURI/binary,BinHeaders/binary>>;
encode(#post{type=Method,uri=URI,headers=Headers,
	     contenttype=ContentType,data=Data},Env) ->
    Type=encode_method_type(Method,post,(Env#env.cap)#cap.extended_methods),
    BinURI=list_to_binary(URI),
    URILen=encode_uintvar(size(BinURI)),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    BinCT=list_to_binary(wsp_bytecodes_headers:encode_content_type(ContentType,
								   Env)),
    HeadersLen=encode_uintvar(size(BinHeaders)+size(BinCT)),
    <<Type:8,URILen/binary,HeadersLen/binary,BinURI/binary,
    BinCT/binary,BinHeaders/binary,Data/binary>>;
encode(#reply{status=Status,headers=Headers,contenttype=ContentType,data=Data},
       Env) ->
    BinStatus=wsp_bytecodes:encode_status(Status),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    BinCT=list_to_binary(wsp_bytecodes_headers:encode_content_type(ContentType,
								   Env)),
    HeadersLen=encode_uintvar(size(BinHeaders)+size(BinCT)),
    <<?Reply:8,BinStatus:8,HeadersLen/binary,BinCT/binary,
    BinHeaders/binary,Data/binary>>;
encode(#push{type=PushType,headers=Headers,contenttype=ContentType,data=Data},
       Env) ->
    Type=encode_push_type(PushType),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    BinCT=list_to_binary(wsp_bytecodes_headers:encode_content_type(ContentType,
								   Env)),
    HeadersLen=encode_uintvar(size(BinHeaders)+size(BinCT)),
    <<Type:8,HeadersLen/binary,BinCT/binary,
    BinHeaders/binary,Data/binary>>;
encode(#suspend{server_sessionid=SessionId},Env) ->
    BinSessionId=encode_uintvar(SessionId),
    <<?Suspend:8,BinSessionId/binary>>;
encode(#resume{server_sessionid=SessionId,capabilities=Cap,
	       headers=Headers},Env) ->
    BinSessionId=encode_uintvar(SessionId),
    BinCap=wsp_bytecodes:encode_capabils(Cap,Env#env.cap),
    CapLen=encode_uintvar(size(BinCap)),
    BinHeaders=
	list_to_binary(wsp_bytecodes_headers:encode_headers(Headers,Env)),
    <<?Resume:8,BinSessionId/binary,CapLen/binary,BinCap/binary,
    BinHeaders/binary>>;
encode(Pdu,_) ->
    ?error("Error when encoding PDU ~p",[Pdu],encode),
    throw({error,cannot_encode_wsp_pdu}).


% Unpack a binary Message PDU into a tuple
% An SPDU is a tuple {Type,Content}
% Binpdu = string()
decode(<<?Connect:8,Major_vers:4,Minor_vers:4,Content/binary>>,Env) ->
    {Content1,CapabilLen}=decode_uintvar(Content),
    {Content2,HeadersLen}=decode_uintvar(Content1),
    {Content3,Cap}=
	wsp_bytecodes:decode_capabils(Content2,CapabilLen,#cap{}),
    {BinHeaders,<<>>}=split_binary(Content3,HeadersLen),
    #connect{version={Major_vers,Minor_vers},capabilities=Cap,
	     headers=wsp_bytecodes_headers:decode_headers(BinHeaders)};
decode(<<?ConnectReply:8,Content/binary>>,Env) ->
    {Content1,SessionId}=decode_uintvar(Content),
    {Content2,CapabilLen}=decode_uintvar(Content1),
    {Content3,HeadersLen}=decode_uintvar(Content2),
    {Content4,Cap}=
	wsp_bytecodes:decode_capabils(Content3,CapabilLen,#cap{}),
    {BinHeaders,<<>>}=split_binary(Content4,HeadersLen),
    #connect_reply{server_sessionid=SessionId,capabilities=Cap,
		   headers=wsp_bytecodes_headers:decode_headers(BinHeaders)};
decode(<<?Redirect:8,Permanent:1,ReuseSecSession:1,0:6,Content/binary>>,Env) ->
    #redirect{permanent=wsp_bytecodes:bin_to_bool(Permanent),
	      reuse_secure_session=wsp_bytecodes:bin_to_bool(ReuseSecSession),
	      rediradresses=decode_addresslist(Content,Env#env.destination)};
decode(<<?Disconnect:8,Content/binary>>,Env) ->
    {Content1,SessionId}=decode_uintvar(Content),
    #disconnect{server_sessionid=SessionId};
decode(<<Type:8,Content/binary>>,Env) when Type==?Get;Type==?Options;
					   Type==?Trace;
					   Type==?Head;Type==?Delete ->
    Method=decode_method_type(Type,get,(Env#env.cap)#cap.extended_methods),
    {Content1,URILen}=decode_uintvar(Content),
    {URI,BinHeaders}=split_binary(Content1,URILen),
    DynHeaders=wsp_bytecodes_headers:decode_headers(BinHeaders),
    Headers=create_headers(Env#env.headers,DynHeaders),
    #get{type=Method,uri=binary_to_list(URI),headers=Headers};
decode(<<Type:8,Content/binary>>,Env) when Type==?Post;Type==?Put ->
    Method=decode_method_type(Type,post,(Env#env.cap)#cap.extended_methods),
    {Content1,URILen}=decode_uintvar(Content),
    {Content2,HeadersLen}=decode_uintvar(Content1),
    {URI,Content3}=split_binary(Content2,URILen),
    {Content4,CTLen,ContentType}=decode_content_type(Content3),
    {BinHeaders,Data}=split_binary(Content4,HeadersLen-CTLen),
    DynHeaders=wsp_bytecodes_headers:decode_headers(BinHeaders),
    Headers=create_headers(Env#env.headers,DynHeaders),
    #post{type=Method,uri=binary_to_list(URI),headers=Headers,
	  contenttype=ContentType,data=Data};
decode(<<?Reply:8,Status:8,Content/binary>>,Env) ->
    {Content1,HeadersLen}=decode_uintvar(Content),
    {Content2,CTLen,ContentType}=decode_content_type(Content1),
    {BinHeaders,Data}=split_binary(Content2,HeadersLen-CTLen),
    DynHeaders=wsp_bytecodes_headers:decode_headers(BinHeaders),
    Headers=create_headers(Env#env.headers,DynHeaders),
    #reply{status=wsp_bytecodes:decode_status(Status),headers=Headers,
	   contenttype=ContentType,data=Data};
decode(<<Type:8,Content/binary>>,Env) when Type==?Push;Type==?ConfirmedPush ->
    {Content1,HeadersLen}=decode_uintvar(Content),
    {Content2,CTLen,ContentType}=decode_content_type(Content1),
    {BinHeaders,Data}=split_binary(Content2,HeadersLen-CTLen),
    DynHeaders=wsp_bytecodes_headers:decode_headers(BinHeaders),
    Headers=create_headers(Env#env.headers,DynHeaders),
    #push{type=decode_push_type(Type),headers=Headers,
	  contenttype=ContentType,data=Data};
decode(<<?Suspend:8,Content/binary>>,Env) ->
    {Content1,SessionId}=decode_uintvar(Content),
    #suspend{server_sessionid=SessionId};
decode(<<?Resume:8,Content/binary>>,Env) ->
    {Content1,SessionId}=decode_uintvar(Content),
    {Content2,CapabilLen}=decode_uintvar(Content1),
    {BinHeaders,Cap}=wsp_bytecodes:decode_capabils(Content2,CapabilLen,
						   Env#env.cap),
    #resume{server_sessionid=SessionId,capabilities=Cap,
	    headers=wsp_bytecodes_headers:decode_headers(BinHeaders)};
decode(Binpdu,Env) ->
    ?error("Error when decoding PDU ~p",[Binpdu],decodePDU),
    throw({error,cannot_decode_wsp_pdu}).


%%% ----------------------------------------------------------------------------
%% Merge the WSP Session headers negotiated during session setup with the
%% specific in this request.
create_headers(StaticHeaders,DynamicHeaders) ->
    lists:append(StaticHeaders,DynamicHeaders).

%%% ----------------------------------------------------------------------------
%%% Encodes Redirect addresses
encode_addresslist([],_) ->
    [];
encode_addresslist([#address{address=Address,bearer=Bearer,port=Port}|List],
		  DestAddr) ->
    {BinBearer,BInc}=
	if
	    Bearer==undefined ->
		{<<>>,0};
	    true ->
		{<<Bearer:8>>,1}
	end,
    {BinPort,PInc}=
	if
	    Port==undefined ->
		{<<>>,0};
	    true ->
		{<<Port:16>>,1}
	end,
    BinAddress=encode_address(Address,DestAddr#address.bearer),
    AddressLen=size(BinAddress),
    <<BInc:1,PInc:1,AddressLen:6,Bearer/binary,Port/binary,BinAddress/binary,
    (encode_addresslist(List,DestAddr))/binary>>.

%%% Decodes redirect addresses
%% Note that the order is reversed!
decode_addresslist(<<>>,_) ->
    [];
decode_addresslist(<<0:1,0:1,AddressLen:6,Content/binary>>,DestAddr) ->
    {Address,Content1}=split_binary(Content,AddressLen),
    [#address{address=decode_address(Address,DestAddr#address.bearer)} |
     decode_addresslist(Content1,DestAddr)];
decode_addresslist(<<0:1,1:1,AddressLen:6,Port:16,Content/binary>>,DestAddr) ->
    {Address,Content1}=split_binary(Content,AddressLen),
    [#address{address=decode_address(Address,DestAddr#address.bearer),
	      port=Port} |
     decode_addresslist(Content1,DestAddr)];
decode_addresslist(<<1:1,0:1,AddressLen:6,Bearer:8,Content/binary>>,DestAddr) ->
    {Address,Content1}=split_binary(Content,AddressLen),
    [#address{address=decode_address(Address,DestAddr#address.bearer),
	      bearer=Bearer} |
     decode_addresslist(Content1,DestAddr)];
decode_addresslist(<<1:1,1:1,AddressLen:6,Bearer:8,Port:16,Content/binary>>,
		   DestAddr) ->
    {Address,Content1}=split_binary(Content,AddressLen),
    [#address{address=decode_address(Address,DestAddr#address.bearer),
	      bearer=Bearer,port=Port} |
     decode_addresslist(Content1,DestAddr)].

%% Encoding depends on bearer. If undefined here, then found in Environ
encode_address({N1,N2,N3,N4},?ANY_ANY_IPv4) ->
    [N1,N2,N3,N4].

%% Encoding depends on bearer
decode_address(<<N1:8,N2:8,N3:8,N4:8>>,?ANY_ANY_IPv4) ->
    {N1,N2,N3,N4}.

%%% ............................................................................
%%% encode/decode of uintvar
encode_uintvar(Int) when Int<128 ->
    <<Int>>;
encode_uintvar(Int) ->
    encode_uintvar(Int,1).

encode_uintvar(_,6) ->
    throw({error,too_big_num});
encode_uintvar(Int,_) when Int<128 ->
    <<0:1,(Int rem 128):7>>;
encode_uintvar(Int,S) ->
    <<1:1,(Int rem 128):7,(encode_uintvar(Int div 128,S+1))/binary>>.


decode_uintvar(Content) ->
    decode_uintvar(Content,0,1).

decode_uintvar(_,Int,6) ->
    throw({error,too_big_num});
decode_uintvar(<<0:1,Int:7,Content/binary>>,Out,S) ->
    {Content,Int+Out};
decode_uintvar(<<1:1,Int:7,Content/binary>>,Out,S) ->
    decode_uintvar(Content,(Int+Out) bsl 7,S+1).


%-------------------------------------------------------------------------------
encode_method_type(get,get,_) -> ?Get;
encode_method_type(options,get,_) -> ?Options;
encode_method_type(head,get,_) -> ?Head;
encode_method_type(delete,get,_) -> ?Delete;
encode_method_type(trace,get,_) -> ?Trace;
encode_method_type(post,post,_) -> ?Post;
encode_method_type(put,post,_) -> ?Put;
encode_method_type(Method,Type,ExtendedMethods) ->
    case lists:keysearch(Method,1,ExtendedMethods) of
	{value,{_,Code,Type}} ->
	    Code;
	Error ->
	    throw({error,invalid_method})
    end.

decode_method_type(?Get,get,_) -> get;
decode_method_type(?Options,get,_) -> options;
decode_method_type(?Head,get,_) -> head;
decode_method_type(?Delete,get,_) -> delete;
decode_method_type(?Trace,get,_) -> trace;
decode_method_type(?Post,post,_) -> post;
decode_method_type(?Put,post,_) -> put;
decode_method_type(Code,get,ExtendedMethods) when 16#50=<Code,Code=<16#5f ->
    case lists:keysearch(Code,1,ExtendedMethods) of
	{value,{Method,_,get}} ->
	    Method;
	Error ->
	    throw({error,invalid_method})
    end;
decode_method_type(Code,post,ExtendedMethods) when 16#70=<Code,Code=<16#7f ->
    case lists:keysearch(Code,2,ExtendedMethods) of
	{value,{Method,_,post}} ->
	    Method;
	Error ->
	    throw({error,invalid_method})
    end;
decode_method_type(_,_,_) ->
    throw({error,invalid_method}).


decode_push_type(push) -> ?Push;
decode_push_type(confirmed_push) -> ?ConfirmedPush.

encode_push_type(?Push) -> push;
encode_push_type(?ConfirmedPush) -> confirmed_push.


%%% Abort reasons used in indications
encode_abort_reason(congestion) ->	?CONGESTION;
encode_abort_reason(user_request) ->	?USERREQ;
encode_abort_reason(proto_error) ->	?WSP_PROTOERR;
encode_abort_reason(disconnect) ->	?DISCONNECT;
encode_abort_reason(suspend) ->		?SUSPEND;
encode_abort_reason(resume) ->		?RESUME;
encode_abort_reason(congestion) ->	?CONGESTION;
encode_abort_reason(connect_error) ->	?CONNECTERR;
encode_abort_reason(mru_exceeded) ->	?MRUEXCEEDED;
encode_abort_reason(mor_exceeded) ->	?MOREXCEEDED;
encode_abort_reason(peer_request) ->	?PEERREQ;
encode_abort_reason(net_error) ->	?NETERR;
encode_abort_reason(X) -> 
    {error,{unknown_abort_reason,X}}.

decode_abort_reason(?CONGESTION) ->	congestion;
decode_abort_reason(?USERREQ) ->	user_request;
decode_abort_reason(?WSP_PROTOERR) ->	proto_error;
decode_abort_reason(?DISCONNECT) ->	disconnect;
decode_abort_reason(?SUSPEND) ->	suspend;
decode_abort_reason(?RESUME) ->		resume;
decode_abort_reason(?CONGESTION) ->	congestion;
decode_abort_reason(?CONNECTERR) ->	connect_error;
decode_abort_reason(?MRUEXCEEDED) ->	mru_exceeded;
decode_abort_reason(?MOREXCEEDED) ->	mor_exceeded;
decode_abort_reason(?PEERREQ) ->	peer_request;
decode_abort_reason(?NETERR) ->		net_error;
decode_abort_reason({wtp,Error}) ->
    R=wtp_pdu:decode_abort_reason(Error),
    {wtp,R};
decode_abort_reason({wsp,Error}) ->
    R=decode_abort_reason(Error),
    {wsp,R};
decode_abort_reason({redirect,Error}) -> {redirect,Error};
decode_abort_reason({reply,Error}) -> {reply,Error};
decode_abort_reason(X) -> 
    {error,{unknown_abort_reason,X}}.


decode_content_type(<<1:1,Code:7,Content/binary>>) ->
    case wsp_bytecodes_headers:decode_content_type(Code+128) of
	{ok,CT} ->
	    {Content,1,CT};
	Error ->
	    throw(Error)
    end;
decode_content_type(Content) ->
    decode_string(Content,[]).


decode_string(<<0:8,Content/binary>>,CT) ->
    list_to_atom(lists:reverse(CT));
decode_string(<<Token:8,Content/binary>>,CT) ->
    decode_string(Content,[Token|CT]).

%%% ============================================================================
%% Pretty prints a decoded WSP PDU

pp_spdu(<<>>) ->
    io:format("---Done with PDU---~n");
pp_spdu(Pdu) when binary(Pdu) ->
    case catch pp_decode(Pdu,#env{}) of
	ok ->
	    io:format("---Done with PDU---~n");
	Error ->
	    io:format("Got Error ~p when decoding ~p~n",[Error,Pdu])
    end;
pp_spdu(Pdu) when list(Pdu) ->
    pp_spdu(list_to_binary(Pdu)).


%% Petty prints a decoded PDU
pp_decode(<<?Connect:8,Major_vers:4,Minor_vers:4,Content/binary>>,Env) ->
    io:format("WSP Type: Connect~n"),
    io:format("  Version: ~p:~p~n",[Major_vers,Minor_vers]),
    {Content1,CapabilLen}=decode_uintvar(Content),
    io:format("    Capabilities length of field: ~p~n",[CapabilLen]),
    {Content2,HeadersLen}=decode_uintvar(Content1),
    io:format("    Headers length of field: ~p~n",[HeadersLen]),
    {Content3,Cap}=wsp_bytecodes:decode_capabils(Content2,CapabilLen,#cap{}),
    wsp_bytecodes:pp_cap(Cap),
    {BinHeaders,<<>>}=split_binary(Content3,HeadersLen),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    ok;
pp_decode(<<?ConnectReply:8,Content/binary>>,Env) ->
    io:format("WSP Type: ConnectReply~n"),
    {Content1,SessionId}=decode_uintvar(Content),
    io:format("  Server Session Id: ~p~n",[SessionId]),
    {Content2,CapabilLen}=decode_uintvar(Content1),
    io:format("    Capabilities length of field: ~p~n",[CapabilLen]),
    {Content3,HeadersLen}=decode_uintvar(Content2),
    io:format("    Headers length of field: ~p~n",[HeadersLen]),
    {Content4,Cap}=wsp_bytecodes:decode_capabils(Content3,CapabilLen,#cap{}),
    wsp_bytecodes:pp_cap(Cap),
    {BinHeaders,<<>>}=split_binary(Content4,HeadersLen),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    ok;
pp_decode(<<?Redirect:8,Permanent:1,ReuseSecSession:1,0:6,Content/binary>>,E) ->
    io:format("WSP Type: Redirect~n"),
    Perm=wsp_bytecodes:bin_to_bool(Permanent),
    ReSec=wsp_bytecodes:bin_to_bool(ReuseSecSession),
    io:format("  Permanent:~p Reuse Secure:~p~n",[Perm,ReSec]),
    io:format("  RedirAddr: ~p~n",[decode_addresslist(Content,
						      E#env.destination)]),
    ok;
pp_decode(<<?Disconnect:8,Content/binary>>,Env) ->
    io:format("WSP Type: Disconnect~n"),
    {Content1,SessionId}=decode_uintvar(Content),
    io:format("  Server Session Id: ~p~n",[SessionId]),
    ok;
pp_decode(<<Type:8,Content/binary>>,Env) when Type==?Get;Type==?Options;
					   Type==?Trace;
					   Type==?Head;Type==?Delete ->
    Method=decode_method_type(Type,get,(Env#env.cap)#cap.extended_methods),
    io:format("WSP Type: ~p~n",[Method]),
    {Content1,URILen}=decode_uintvar(Content),
    io:format("  URI length of field: ~p~n",[URILen]),
    {URI,BinHeaders}=split_binary(Content1,URILen),
    io:format("  URI: ~p~n",[binary_to_list(URI)]),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    ok;
pp_decode(<<Type:8,Content/binary>>,Env) when Type==?Post;Type==?Put ->
    Method=decode_method_type(Type,post,(Env#env.cap)#cap.extended_methods),
    io:format("WSP Type: ~p~n",[Method]),
    {Content1,URILen}=decode_uintvar(Content),
    io:format("  URI length of field: ~p~n",[URILen]),
    {Content2,HeadersLen}=decode_uintvar(Content1),
    io:format("    Headers length of field: ~p~n",[HeadersLen]),
    {URI,Content3}=split_binary(Content2,URILen),
    io:format("  URI: ~p~n",[binary_to_list(URI)]),
    {Content4,CTLen,ContentType}=decode_content_type(Content3),
    io:format("  ContentType: ~p~n",[ContentType]),
    {BinHeaders,Data}=split_binary(Content4,HeadersLen-CTLen),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    io:format("  length(Body):~p ~p~n",[size(Data),Data]),
    ok;
pp_decode(<<?Reply:8,Status:8,Content/binary>>,Env) ->
    io:format("WSP Type: Reply~n"),
    io:format("  Status: ~p~n",[wsp_bytecodes:decode_status(Status)]),
    {Content1,HeadersLen}=decode_uintvar(Content),
    io:format("    Headers length of field: ~p~n",[HeadersLen]),
    {Content2,CTLen,ContentType}=decode_content_type(Content1),
    io:format("  ContentType: ~p~n",[ContentType]),
    {BinHeaders,Data}=split_binary(Content2,HeadersLen-CTLen),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    io:format("  length(Body):~p ~p~n",[size(Data),Data]),
    ok;
pp_decode(<<Type:8,Content/binary>>,Env) when Type==?Push;
					      Type==?ConfirmedPush ->
    io:format("WSP Type: ~p~n",[decode_push_type(Type)]),
    {Content1,HeadersLen}=decode_uintvar(Content),
    io:format("    Headers length of field: ~p~n",[HeadersLen]),
    {Content2,CTLen,ContentType}=decode_content_type(Content1),
    io:format("  ContentType: ~p~n",[ContentType]),
    {BinHeaders,Data}=split_binary(Content2,HeadersLen-CTLen),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    ok;
pp_decode(<<?Suspend:8,Content/binary>>,Env) ->
    io:format("WSP Type: Suspend~n"),
    {Content1,SessionId}=decode_uintvar(Content),
    io:format("  Server Session Id: ~p~n",[SessionId]),
    ok;
pp_decode(<<?Resume:8,Content/binary>>,Env) ->
    io:format("WSP Type: Resume~n"),
    {Content1,SessionId}=decode_uintvar(Content),
    io:format("  Server Session Id: ~p~n",[SessionId]),
    {Content2,CapabilLen}=decode_uintvar(Content1),
    io:format("    Capabilities length of field: ~p~n",[CapabilLen]),
    {BinHeaders,Cap}=wsp_bytecodes:decode_capabils(Content2,CapabilLen,
						   Env#env.cap),
    wsp_bytecodes:pp_cap(Cap),
    wsp_bytecodes_headers:pp_headers(BinHeaders),
    ok;
pp_decode(Binpdu,Env) ->
    throw({error,cannot_decode_wsp_pdu}).

