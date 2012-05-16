%%% File    : wtp_bytecodes.hrl
%%% Purpose : Assigned Numbers in WTP

-revision('$Revision: 1.1.1.1 $ ').
-rcsid('@(#) $Id: wtp_bytecodes.hrl,v 1.1.1.1 2001/07/04 14:51:15 uid56739 Exp $ ').

%%% ----------------------------------------------------------------------------
%%% All definitions from:
%%% - WAP-201-WTP Approved version 19-Feb-2000 (June 2000 Release)

%%% WTP PDU Types, Table 13
-define(Concatenated_Indicator,0).
-define(Invoke,		16#1).
-define(Result,		16#2).
-define(Ack,		16#3).
-define(Abort,		16#4).
-define(SegInvoke,	16#5).
-define(SegResult,	16#6).
-define(Nack,		16#7).

%%% Encoding of Class field, Table 16
-define(CLASS0,		16#0).
-define(CLASS1,		16#1).
-define(CLASS2,		16#2).

%% WTP Abort Types, Table 20
-define(PROVIDER,	16#0).
-define(USER,		16#1).

%% WTP Provider Abort Codes, Table 21
-define(UNKNOWN,	    16#0).
-define(WTP_PROTOERR,	    16#1).
-define(INVALIDTID,	    16#2).
-define(NOTIMPLEMENTEDCL2,  16#3).
-define(NOTIMPLEMENTEDSAR,  16#4).
-define(NOTIMPLEMENTEDUACK, 16#5).
-define(WTPVERSIONZERO,	    16#6).
-define(CAPTEMPEXCEEDED,    16#7).
-define(NORESPONSE,	    16#8).
-define(MESSAGETOOLARGE,    16#9).

%% Encodings of TPIs, Table 24
-define(TpiError,	16#0).
-define(TpiInfo,	16#1).
-define(TpiOption,	16#2).
-define(TpiPSN,		16#3).

%% Encoding of Error TPI, Table 25
-define(UNKNOWN_TPI,	16#1).
-define(KNOWN_TPI,	16#2).

%% Encoding of Option TPI, Table 29
-define(MAXIMUM_RECEIVE_UNIT,	 16#1).
-define(TOTAL_MESSAGE_SIZE,	 16#2).
-define(DELAY_TRANSMISSION_TIMER,16#3).
-define(MAXIMUM_GROUP,		 16#4).
-define(CURRENT_TID,		 16#5).
-define(NO_CACHED_TID,		 16#6).
