%%% File    : wdp_bytecodes.hrl
%%% Purpose : Assigned Numbers in WDP

-revision('$Revision: 1.1.1.1 $\n').
-rcsid('@(#) $Id: wdp_bytecodes.hrl,v 1.1.1.1 2001/07/04 14:51:16 uid56739 Exp $\n').

%%% ----------------------------------------------------------------------------
%%% All definitions from:
%%% - WAP-200-WDP Approved version 19-Feb-2000 (June 2000 Release)

%%% Port Number Definitions, Appendix B
-define(WAP_WTA_WSP_S,2805).
-define(WAP_WTA_WSP_WTP_S,2923).
-define(WAP_PUSH,2948).
-define(WAP_PUSH_S,2949).
-define(WAP_WSP,9200).
-define(WAP_WSP_S,9202).
-define(WAP_WSP_WTP,9201).
-define(WAP_WSP_WTP_S,9203).
-define(WAP_VCARD,9004).
-define(WAP_VCARD_S,9006).
-define(WAP_VCAL,9005).
-define(WAP_VCAL_S,9007).

%%% Bearer Type Assignments, Appendix C
%%% Names derived from: Network - Bearer type - Address type
-define(ANY_ANY_IPv4,		     16#0).
-define(ANY_ANY_IPv6,		     16#1).
-define(GSM_USSD_ANY,		     16#2).
-define(GSM_SMS_GSMMSISDN,	     16#3).
-define(ANSI136_GUTS_ANSI136MSISDN,  16#4).
-define(IS95CDMA_SMS_IS637MSISDN,    16#5).
-define(IS95CDMA_CSD_IPv4,	     16#6).
-define(IS95CDMA_Packet_IPv4,	     16#7).
-define(ANSI136_CSD_IPv4,	     16#8).
-define(ANSI136_Packet_IPv4,	     16#9).
-define(GMS_CSD_IPv4,		     16#a).
-define(GMS_GPRS_IPv4,		     16#b).
-define(GMS_USSD_IPv4,		     16#c).
-define(AMPS_CDPD_IPv4,		     16#d).
-define(PDC_CSD_IPv4,		     16#e).
-define(PDC_Packet_IPv4,	     16#f).
-define(IDEN_SMS_iDENMSISDN,	     16#10).
-define(IDEN_CSD_IPv4,		     16#11).
-define(IDEN_Packet_IPv4,	     16#12).
-define(Paging_FLEX_FLEXMSISDN,	     16#13).
-define(PHS_SMS_PHSMSISDN,	     16#14).
-define(PHS_CSD_IPv4,		     16#15).
-define(GMS_USSD_GSMService,	     16#16).
-define(TETRA_SDS_TETRAITSI,	     16#17).
-define(TETRA_SDS_TETRAMSISDN,	     16#18).
-define(TETRA_Packet_IPv4,	     16#19).
-define(Paging_ReFLEX_ReFLEXMSISDN,  16#1a).
-define(GSM_USSD_GSMMSISDN,	     16#1b).
-define(Mobitex_MPAK_MAN,	     16#1c).
-define(ANSI136_GHOSTRDATA_GSMMSISDN,16#1d).
