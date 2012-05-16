%% File    : wtp_pdu.erl
%%% Author  : Johan Blom <johblo@dragon.cellpt.se>
%%% Purpose : Bit coding/decoding of WTP PDUs
%%% Created : 26 Jul 2000 by Johan Blom <johblo@dragon.cellpt.se>

-module(wtp_pdu).
-author('johblo@dragon.cellpt.se').
-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wtp_pdu.erl,v 1.1.1.1 2001/07/04 14:51:15 uid56739 Exp $ ').
-modified('$Date: 2001/07/04 14:51:15 $ ').
-modified_by('$Author: uid56739 $ ').
-vsn("1").

-export([decode_pdu/1,encode_pdu/1,
	 lookup_tpi/2,
	 pp_tpdu/1]).

-export([decode_abort_reason/1]).

-include("wtp.hrl").

%% =============================================================================
%% For now, never Concatenate WTP packets nor use segmentation.
encode_pdu(#invoke_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,version=Version,
		       tidnew=TIDNew,uack=Uack,tcl=TCL,
		       tpilist=TPIList,data=Data}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    <<CON:1,?Invoke:4,GTR:1,TTR:1,RID:1,TID:16,Version:2,
     TIDNew:1,Uack:1,0:2,TCL:2,BinTPI/binary,Data/binary>>;
encode_pdu(#result_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,
		       tpilist=TPIList,data=Data}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    <<CON:1,?Result:4,GTR:1,TTR:1,RID:1,TID:16,BinTPI/binary,Data/binary>>;
encode_pdu(#ack_pdu{tidver=TIDver,rid=RID,tid=TID,tpilist=TPIList}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    <<CON:1,?Ack:4,TIDver:1,0:1,RID:1,TID:16,BinTPI/binary>>;
encode_pdu(#abort_pdu{type=AbortType,reason=AbortReason,tid=TID,
		      tpilist=TPIList}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    <<CON:1,?Abort:4,AbortType:3,TID:16,AbortReason:8,BinTPI/binary>>;
encode_pdu(#seginvoke_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,psn=PacketSeqNum,
			  tpilist=TPIList,segment=Segment}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    <<CON:1,?SegResult:4,GTR:1,TTR:1,RID:1,TID:16,PacketSeqNum:8,
    BinTPI/binary,Segment/binary>>;
encode_pdu(#segresult_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,psn=PacketSeqNum,
			  tpilist=TPIList,segment=Segment}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    <<CON:1,?SegResult:4,GTR:1,TTR:1,RID:1,TID:16,PacketSeqNum:8,
    BinTPI/binary,Segment/binary>>;
encode_pdu(#nack_pdu{rid=RID,tid=TID,misspack=NumMissingPacket,
		     numlist=NumList,tpilist=TPIList}) ->
    {CON,BinTPI}=encode_pdutpi(TPIList),
    BinNumList=list_to_binary(NumList),
    <<CON:1,?Nack:4,0:2,RID:1,TID:16,NumMissingPacket:8,
    BinNumList/binary,BinTPI/binary>>;
encode_pdu(_) ->
    throw({error,cannot_encode_pdu}).

encode_pdutpi(TPIList) ->
    BinTPI=encode_TPI2(TPIList),
    CON=case BinTPI of <<>> -> ?FALSE; _ -> ?TRUE end,
    {CON,BinTPI}.

encode_TPI2([]) ->
    <<>>;
encode_TPI2([#error_tpi{code=known_tpi,badTPI=BadTPI,first=FirstOctet}|Rest]) ->
    CON=case Rest of [] -> ?FALSE; _ -> ?TRUE end,
    <<CON:1,?TpiError:4,?FALSE:1,1:2,?KNOWN_TPI:4,BadTPI:4,FirstOctet:8,
    (encode_TPI2(Rest))/binary>>;
encode_TPI2([#error_tpi{code=unknown_tpi,badTPI=BadTPI}|Rest]) ->
    CON=case Rest of [] -> ?FALSE; _ -> ?TRUE end,
    <<CON:1,?TpiError:4,?FALSE:1,1:2,?UNKNOWN_TPI:4,BadTPI:4,
    (encode_TPI2(Rest))/binary>>;
encode_TPI2([#option_tpi{optionTPI=OptionIdentity,value=Value}|Rest]) ->
    CON=case Rest of [] -> ?FALSE; _ -> ?TRUE end,
    Length=length(Value)+1,
    <<CON:1,?TpiOption:4,?FALSE:1,Length:2,OptionIdentity:8,Value/binary,
    (encode_TPI2(Rest))/binary>>;
encode_TPI2([{info_tpi,Info}|Rest]) ->
    CON=case Rest of [] -> ?FALSE; _ -> ?TRUE end,
    Length=length(Info),
    <<CON:1,?TpiInfo:4,?FALSE:1,Length:2,Info/binary,
    (encode_TPI2(Rest))/binary>>;
encode_TPI2([{psn_tpi,PacketSeqNum}|Rest]) ->
    CON=case Rest of [] -> ?FALSE; _ -> ?TRUE end,
    <<CON:1,?TpiPSN:4,?FALSE:1,1:2,PacketSeqNum:8,
    (encode_TPI2(Rest))/binary>>.


decode_pdu(List) when list(List) ->
    decode_pdu(list_to_binary(List));

decode_pdu(<<?Concatenated_Indicator:8,0:1,Length:7,Content/binary>>) ->
    PDUlist=decode_concatenated_pdu(Length,Content),
    {concatenated,PDUlist};
decode_pdu(<<CON:1,?Invoke:4,GTR:1,TTR:1,RID:1,TID:16,
	   Version:2,TIDNew:1,Uack:1,RES:2,TCL:2,Content/binary>>) ->
    {Data,TPIList}=decode_pdutpi(CON,Content),
    #invoke_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,version=Version,tidnew=TIDNew,
		uack=Uack,tcl=TCL,tpilist=TPIList,data=Data};
decode_pdu(<<CON:1,?Result:4,GTR:1,TTR:1,RID:1,TID:16,
	   Content/binary>>) ->
    {Data,TPIList}=decode_pdutpi(CON,Content),
    #result_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,tpilist=TPIList,data=Data};
decode_pdu(<<CON:1,?Ack:4,TIDver:1,RES:1,RID:1,TID:16,Content/binary>>) ->
    {<<>>,TPIList}=decode_pdutpi(CON,Content),
    #ack_pdu{tidver=TIDver,rid=RID,tid=TID,tpilist=TPIList};
decode_pdu(<<CON:1,?Abort:4,AbortType:3,TID:16,AbortReason:8,Content/binary>>)->
    {<<>>,TPIList}=decode_pdutpi(CON,Content),
    #abort_pdu{type=AbortType,reason=AbortReason,tid=TID,tpilist=TPIList};
decode_pdu(<<CON:1,?SegInvoke:4,GTR:1,TTR:1,RID:1,TID:16,
	   PacketSeqNum:8,Content/binary>>) ->
    {Segment,TPIList}=decode_pdutpi(CON,Content),
    #seginvoke_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,psn=PacketSeqNum,
		   tpilist=TPIList,segment=Segment};
decode_pdu(<<CON:1,?SegResult:4,GTR:1,TTR:1,RID:1,TID:16,
	   PacketSeqNum:8,Content/binary>>) ->
    {Segment,TPIList}=decode_pdutpi(CON,Content),
    #segresult_pdu{gtr=GTR,ttr=TTR,rid=RID,tid=TID,psn=PacketSeqNum,
		    tpilist=TPIList,segment=Segment};
decode_pdu(<<CON:1,?Nack:4,RES:2,RID:1,TID:16,
	   NumMissingPacket:8,Content/binary>>) ->
    {BinNumList,Content2}=split_binary(Content,NumMissingPacket),
    {<<>>,TPIList}=decode_pdutpi(CON,Content2),
    #nack_pdu{rid=RID,tid=TID,misspack=NumMissingPacket,
	      numlist=binary_to_list(BinNumList),tpilist=TPIList};
decode_pdu(_) ->
    throw({error,cannot_decode_pdu}).


%%% Unpack the TPI part of a WTP PDU if the CON flag is set, and continue
%%% decode TPIs as long as this is true.
%%% Returns a list with parsed TPIs that may contain:
%%% - error_tpi, option_tpi records
%%% - info_tpi, psn_tpi tuples
%%% - error tuple with error_tpi record, if parsing of any TPI failed. 
decode_pdutpi(CON,Content) ->
    decodeTPI2(CON,Content,[]).
    
decodeTPI2(?FALSE,Content,Out) ->
    {Content,lists:reverse(Out)};
decodeTPI2(?TRUE,
	   <<CON:1,?TpiError:4,?FALSE:1,1:2,?UNKNOWN_TPI:4,BadTPI:4,
	   Content/binary>>,Out) ->
    decodeTPI2(CON,Content,[#error_tpi{code=unknown_tpi,
				       badTPI=BadTPI}|Out]);
decodeTPI2(?TRUE,
	   <<CON:1,?TpiError:4,?FALSE:1,2:2,?KNOWN_TPI:4,BadTPI:4,FirstOctet:8,
	   Content/binary>>,Out) ->
    decodeTPI2(CON,Content,[#error_tpi{code=known_tpi,
				       badTPI=BadTPI,
				       first=FirstOctet}|Out]);
decodeTPI2(?TRUE,
	   <<CON:1,?TpiOption:4,?FALSE:1,Length:2,OptionIdentity:8,
	   Content/binary>>,Out) ->
    {Value,Content2}=split_binary(Content,Length-1),
    decodeTPI2(CON,Content,[#option_tpi{optionTPI=OptionIdentity,
					value=Value}|Out]);
decodeTPI2(?TRUE,
	   <<CON:1,?TpiInfo:4,?FALSE:1,Length:2,Content/binary>>,Out) ->
    {Info,Content2}=split_binary(Content,Length),
    decodeTPI2(CON,Content,[{info_tpi,Info}|Out]);
decodeTPI2(?TRUE,
	   <<CON:1,?TpiPSN:4,?FALSE:1,1:2,PacketSeqNum:8,
	   Content/binary>>,Out) ->
    decodeTPI2(CON,Content,[{psn_tpi,PacketSeqNum}|Out]);
decodeTPI2(?TRUE,<<CON:1,Identity:4,?FALSE:1,Length:2,
	   Content/binary>>,Out) ->
    {Content,Code,First}=parse_tpi(Identity,Length,Content),
    {error,#error_tpi{code=Code,badTPI=Identity,first=First}};
decodeTPI2(?TRUE,<<CON:1,Identity:4,?TRUE:1,RES:2,Length:8,
	   Content/binary>>,Out) ->
    {Content2,Code,First}=parse_tpi(Identity,Length,Content),
    {error,#error_tpi{code=Code,badTPI=Identity,first=First}}.


parse_tpi(Identity,Length,Content) ->
    case lists:member(Identity,[?TpiError,?TpiInfo,?TpiOption,?TpiPSN]) of
	true ->
	    {Value,Content2}=split_binary(Content,Length),
	    {Content2,?KNOWN_TPI,[]};
	false ->
	    {Value,<<First:8,Content2/binary>>}=split_binary(Content,Length),
	    {Content2,?UNKNOWN_TPI,First}
    end.


%%% Decodes concatenated PDUs
%%% Returns a list with parsed PDUs, order of these are not significant
decode_concatenated_pdu(Length,Content) ->
    case split_binary(Content,Length) of
	{WTPpdu,<<>>} ->
	    decode_pdu(WTPpdu);
	{WTPpdu,<<0:1,Length2:7,Content2/binary>>} ->
	    [decode_pdu(WTPpdu)|decode_concatenated_pdu(Length2,Content2)]
    end.

%% -----------------------------------------------------------------------------
%% Looks up a Specific TPI in a list of TPIs, from decode_pdutpi/2
lookup_tpi(Type,Tpilist) ->
    case lists:keysearch(Type,1,Tpilist) of
	{value,{Type,Tpi}} -> Tpi;
	_ -> []
    end.

%-------------------------------------------------------------------------------
decode_abort_reason(wtp,Code) ->
    decode_abort_reason(Code);
decode_abort_reason(wsp,Code) ->
    wsp_pdu:decode_abort_reason(Code).

decode_abort_reason(?UNKNOWN) ->		unknown;
decode_abort_reason(?WTP_PROTOERR) ->		wtp_protoerr;
decode_abort_reason(?INVALIDTID) ->		invalid_tid;
decode_abort_reason(?NOTIMPLEMENTEDCL2) ->	not_implemented_class2;
decode_abort_reason(?NOTIMPLEMENTEDSAR) ->	not_implemented_sar;
decode_abort_reason(?NOTIMPLEMENTEDUACK) ->	not_implemented_uack;
decode_abort_reason(?WTPVERSIONZERO) ->		wtp_version_zero;
decode_abort_reason(?CAPTEMPEXCEEDED) ->	captemp_exceeded;
decode_abort_reason(?NORESPONSE) ->		no_response;
decode_abort_reason(?MESSAGETOOLARGE) ->	message_too_large;
decode_abort_reason(A) ->                       {illegal_error_code,A}.


%%% ============================================================================
%% Pretty prints a decoded WTP PDU
pp_tpdu(List) when list(List) ->
    {ok,Spdu}=pp_tpdu2(list_to_binary(List)),
    wsp_pdu:pp_spdu(Spdu);
pp_tpdu(BinPdu) ->
    {ok,Spdu}=pp_tpdu2(BinPdu),
    wsp_pdu:pp_spdu(Spdu).

pp_tpdu2(<<CON:1,?Invoke:4,GTR:1,TTR:1,RID:1,TID:16,
	   Version:2,TIDNew:1,Uack:1,RES:2,TCL:2,Content/binary>>) ->
    io:format("WTP Invoke: Tid=~p GTR:~p  TTR:~p  RID:~p~n",[TID,GTR,TTR,RID]),
    io:format("        Version: ~p~n",[Version]),
    io:format("        TidNew:~p  Uack:~p  TCL: ~p~n",[TIDNew,Uack,TCL]),
    {Data,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,Data};
pp_tpdu2(<<CON:1,?Result:4,GTR:1,TTR:1,RID:1,TID:16,Content/binary>>) ->
    io:format("WTP Result: Tid=~p GTR=~p  TTR=~p  RID=~p~n",[TID,GTR,TTR,RID]),
    {Data,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,Data};
pp_tpdu2(<<CON:1,?Ack:4,TIDver:1,RES:1,RID:1,TID:16,Content/binary>>) ->
    io:format("WTP Ack: Tid=~p Tve/Tok=~p  RID=~p~n",[TID,TIDver,RID]),
    {Data,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,[]};
pp_tpdu2(<<CON:1,?Abort:4,AbortType:3,TID:16,AbortReason:8,Content/binary>>) ->
    AT=if
	   AbortType==?PROVIDER -> wtp;
	   AbortType==?USER -> wsp
       end,
    io:format("WTP Abort: Tid=~p Reason=~p,~p~n",
	      [TID,AT,decode_abort_reason(AT,AbortReason)]),
    {Data,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,[]};
pp_tpdu2(<<CON:1,?SegInvoke:4,GTR:1,TTR:1,RID:1,TID:16,
	   PacketSeqNum:8,Content/binary>>) ->
    io:format("WTP SegInvoke: Tid=~p GTR=~p  TTR=~p  RID=~p~n",
	      [TID,GTR,TTR,RID]),
    io:format("           PacketSequenceNumber: ~p~n",[PacketSeqNum]),
    {Segment,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,Segment};
pp_tpdu2(<<CON:1,?SegResult:4,GTR:1,TTR:1,RID:1,TID:16,
	 PacketSeqNum:8,Content/binary>>) ->
    io:format("WTP SegResult: Tid=~p GTR=~p  TTR=~p  RID=~p~n",
	      [TID,GTR,TTR,RID]),
    io:format("           PacketSequenceNumber=~p~n",[PacketSeqNum]),
    {Segment,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,Segment};
pp_tpdu2(<<CON:1,?Nack:4,RES:2,RID:1,TID:16,
	   NumMissingPacket:8,Content/binary>>) ->
    {BinNumList,Content2}=split_binary(Content,NumMissingPacket),
    io:format("WTP Nack: Tid=~p RID=~p NumMissPackets=~p Numlist=~p~n",
	      [TID,RID,NumMissingPacket,BinNumList]),
    {Data,TPIList}=decode_pdutpi(CON,Content),
    pp_tpilist(TPIList),
    {ok,[]};
pp_tpdu2(B) ->
    io:format("  ERROR: ~p~n",[B]),
    {ok,[]}.


pp_tpilist([]) ->
    [];
pp_tpilist([#error_tpi{code=unknown_tpi,badTPI=BadTPI}|TPIList]) ->
    io:format("      ErrorTPI with unknown TPI~n",[]),
    io:format("          Bad TPI code:~p~n",[BadTPI]),
    pp_tpilist(TPIList);
pp_tpilist([#error_tpi{code=known_tpi,badTPI=BadTPI,first=FirstOct}|TPIList]) ->
    io:format("      ErrorTPI with known TPI~n",[]),
    io:format("          Bad TPI code:~p  First Octet:~p~n",[BadTPI,FirstOct]),
    pp_tpilist(TPIList);
pp_tpilist([#option_tpi{optionTPI=OptionIdentity,value=Value}|TPIList]) ->
    io:format("      OptionTPI~n",[]),
    io:format("          Option:~p  Value:~p~n",[OptionIdentity,Value]),
    pp_tpilist(TPIList);
pp_tpilist([{info_tpi,Info}|TPIList]) ->
    io:format("      InfoTPI~n",[]),
    io:format("          Info:~p~n",[Info]),
    pp_tpilist(TPIList);
pp_tpilist([{psn_tpi,PacketSeqNum}|TPIList]) ->
    io:format("      PSNTPI~n",[]),
    io:format("          Packet Sequence Number:~p~n",[PacketSeqNum]),
    pp_tpilist(TPIList).
