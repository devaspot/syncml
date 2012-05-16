-revision('$Revision: 1.2 $ ').
-rcsid('@(#) $Id: wsp.hrl,v 1.2 2001/07/09 12:37:30 johblo Exp $ ').

-include("wsp_bytecodes.hrl").
-include("wtp_bytecodes.hrl").
-include("wdp_bytecodes.hrl").
-include("stacklog.hrl").

-ifndef(START_OPTIONS).
% -define(START_OPTIONS,       [{debug,[trace]}]).
-define(START_OPTIONS,       []).
-endif.

-define(FALSE,0).
-define(TRUE,1).

%%% Redirect flags
-define(PermanentRedirect,	16#80).
-define(ReuseSecuritySession,	16#40).

%%% Curent version of WSP protocol implemented.
-define(WSP_VERSION,{1,0}).
    
%%% Default WSP Header Code Page.
-define(DEFAULT_PAGE,1).

%%% Inactivity Timeout of a session
-define(SESSION_TIMEOUT, 5000).

%%% 
-define(pseudo_disconnect(WSPses),
	gen_fsm:sync_send_event(WSPses, pseudo_disconnect)).
-define(pseudo_suspend(WSPses),
	gen_fsm:sync_send_event(WSPses, pseudo_suspend)).

%%% WSP specific record definitions
%%% WSP 
-record(cap,{client_sdu=1400,     % (uintvar) Max size client can receive
	     server_sdu=1400,     % (uintvar) Max size server can receive
	     cpushf=false,        % (bool) Support of Confirmed Push
	     pushf=false,         % (bool) Support of Push
	     sresumef=false,      % (bool) Support of Session Resume
	     ackhead=false,       % (bool) Support of Ack Headers
	     mom=1,               % (uint8) Max outstanding methods
	     mop=1,               % (uint8) Max outstanding pushes
	     extended_methods=[], % (list) 
	     header_code_pages=[],% (list)
	     aliases=[],          % (list)
	     other=[]             % (list) Unknown capabilities
	     }).

-record(address,{bearer=?ANY_ANY_IPv4, % (uint8)
		 port,                 % (uint16)
		 address               % bearer dependent
		 }).


-record(env,{encoding_version=[],    % (key/val list) Encoding Versions to use
	     cap=#cap{},             % (#cap) Current set of capabilities
	     destination=#address{}, % (#addr) Destination address/port/bearer
	     headers=[]         % (key/val list) Static headers to .ind from WTP
	    }).

%%% WSP PDUs
-record(connect,{version=?WSP_VERSION,% ({uint4,uint4}) WSP version
		 capabilities,        % (#cap) Capabilities
		 headers=[]           % (key-val list) Headers
		}).

-record(connect_reply,{server_sessionid,% (uintvar)
		       capabilities,    % (#cap) Capabilities
		       headers=[]       % (key-val list) Headers
		      }).

-record(redirect,{permanent,            % (bool) Permanent redirection
		  reuse_secure_session, % (bool) Reuse WTLS session
		  rediradresses         % (list with #address)
		 }).

-record(disconnect,{server_sessionid % (uintvar)
		   }).

-record(get,{type=get,  % (atom) Type of get PDU
	     uri,       % (string) URI
	     headers=[] % (key-val list) Headers
	    }).

-record(post,{type=post,      % (atom) Type of post PDU
	      uri,            % (string) URI
	      contenttype='', % (atom) Content-Type
	      headers=[],     % (key-val list) Headers
	      data=[]         % (binary) Data
	    }).

-record(reply,{status,         % (uint8) HTTP Status code, WAP encoded
	       contenttype='', % (atom) Content-Type
	       headers=[],     % (key-val list) Headers
	       data=[]         % (binary) Data
	      }).

-record(push,{type,           % (atom) Type of post PDU (push or confirmed_push)
	      contenttype='', % (atom) Content-Type
	      headers=[],     % (key-val list) Headers
	      data=[]         % (binary) Data
	     }).

-record(suspend,{server_sessionid % (uintvar)
		}).

-record(resume,{server_sessionid,% (uintvar)
		capabilities,    % (#cap) Capabilities
		headers=[]       % (key-val list) Headers
	       }).

