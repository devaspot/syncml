-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wtp.hrl,v 1.2 2001/07/09 12:38:57 johblo Exp $ ').

-include("wtp_bytecodes.hrl").
-include("stacklog.hrl").


-ifndef(START_OPTIONS).
% -define(START_OPTIONS,       [{debug,[trace]}]).
-define(START_OPTIONS,       []).
-endif.


%% Curent version of the WTP protocol implemented.
-define(WTP_VERSION,0).


%% Other defines used in coding of PDUs
-define(FALSE,0).% Bit set to 0
-define(TRUE,1). % Bit set to 1


%%% WTP specific record definitions
-record(invoke_pdu,{gtr=?TRUE,  % (bool) Group trailer flag
		    ttr=?TRUE,  % (bool) Transmission trailer flag
		    rid=?FALSE, % (bool) Re-transmission indicator
		    tid,        % (uint16) Transaction Identifier
		    version=?WTP_VERSION,% (uint8) WTP version
		    tidnew=?FALSE,       % (bool) True if wrapped tid value
		    uack=?TRUE,          % (bool) Require User Acknowledge
		    tcl,       % (uint2) Transaction Class
		    tpilist=[],% (list) List with TPIs
		    data=[]    % (list) Data
		   }).

-record(result_pdu,{gtr=?TRUE, % (bool) Group trailer flag
		    ttr=?TRUE, % (bool) Transmission trailer flag
		    rid=?FALSE,% (bool) Re-transmission indicator
		    tid,       % (uint16) Transaction Identifier
		    tpilist=[],% (list) List with TPIs
		    data=[]    % (list) Data
		   }).

-record(ack_pdu,{tidver=?FALSE, % (bool) Tid verification flag
		 rid=?FALSE,    % (bool) Re-transmission indicator
		 tid,           % (uint16) Transaction Identifier
		 tpilist=[]     % (list) List with TPIs
		}).

-record(abort_pdu,{type,      % (uint4) Abort Type
		   reason,    % (uint4) Abort Reason
		   tid,       % (uint16) Transaction Identifier
		   tpilist=[] % (list) List with TPIs
		  }).

-record(seginvoke_pdu,{gtr=?FALSE,% (bool) Group trailer flag
		       ttr=?FALSE,% (bool) Transmission trailer flag
		       rid=?FALSE,% (bool) Re-transmission indicator
		       tid,       % (uint16) Transaction Identifier
		       psn,       % (uint8) Packet Sequence Number
		       tpilist=[],% (list) List with TPIs
		       segment    % (list) Data segment
		      }).

-record(segresult_pdu,{gtr=?FALSE,% (bool) Group trailer flag
		       ttr=?FALSE,% (bool) Transmission trailer flag
		       rid=?FALSE,% (bool) Re-transmission indicator
		       tid,       % (uint16) Transaction Identifier
		       psn,       % (uint8) Packet Sequence Number
		       tpilist=[],% (list) List with TPIs
		       segment    % (list) Data segment
		      }).

-record(nack_pdu,{rid=?FALSE,% (bool) Re-transmission indicator
		  tid,       % (uint16) Transaction Identifier
		  misspack,  % (uint8) Number of missing packets
		  numlist=[],% (list) Packet Sequence Numbers of Missing Packets
		  tpilist=[] % (list) List with TPIs
		 }).


%% Note that the info_tpi and psn_tpi are represented as ordinary tuples with a
%% single value.
-record(error_tpi,{code,   % Error Code 
		   badTPI, % Identity of bad TPI
		   first   % First octet, if unknown identity
		  }).
-record(option_tpi,{optionTPI, % Option Identity
		    value      % Option Value
		   }).

