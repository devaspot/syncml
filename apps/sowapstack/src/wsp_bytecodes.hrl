%%% File    : wsp_bytecodes.hrl
%%% Purpose : Assigned Numbers in WSP

-revision('$Revision: 1.1.1.1 $\n').
-rcsid('@(#) $Id: wsp_bytecodes.hrl,v 1.1.1.1 2001/07/04 14:51:13 uid56739 Exp $\n').

%%% ----------------------------------------------------------------------------
%%% All definitions from:
%%% - WAP-203-WSP Approved version 4-May-2000 (June 2000 Release)
%%% - WAP-203-001-WSP Specification Information Note 20-June-2000

%% PDU Type Assignments
%% Appendix A, Table 34, Page 94
-define(Connect,	16#1).
-define(ConnectReply,	16#2).
-define(Redirect,	16#3).
-define(Reply,		16#4).
-define(Disconnect,	16#5).
-define(Push,		16#6).
-define(ConfirmedPush,	16#7).
-define(Suspend,	16#8).
-define(Resume,		16#9).
-define(Get,		16#40).
-define(Options,	16#41).
-define(Head,		16#42).
-define(Delete,		16#43).
-define(Trace,		16#44).
-define(Post,		16#60).
-define(Put,		16#61).

%% Abort Reason Code Assignments
%% Appendix A, Table 35, Page 95
-define(WSP_PROTOERR,	16#e0).
-define(DISCONNECT,	16#e1).
-define(SUSPEND,	16#e2).
-define(RESUME,		16#e3).
-define(CONGESTION,	16#e4).
-define(CONNECTERR,	16#e5).
-define(MRUEXCEEDED,	16#e6).
-define(MOREXCEEDED,	16#e7).
-define(PEERREQ,	16#e8).
-define(NETERR,		16#e9).
-define(USERREQ,	16#ea).
-define(USERRFS,	16#eb). % Push only
-define(USERPND,	16#ec).
-define(USERDCR,	16#ed).
-define(USERDCU,	16#ee). % Push only

%% Capability Assignments
%% Appendix A, Table 37, Page 106, WSP-Version-6-November-2000
-define(Client_SDU_Size,16#00).
-define(Server_SDU_Size,16#01).
-define(ProtocolOptions,16#02).
-define(Method_MOR,	16#03).
-define(Push_MOR,	16#04).
-define(ExtendedMethods,16#05).
-define(HeaderCodePages,16#06).
-define(Aliases,	16#07).
-define(ClientMessageSize,16#08).
-define(ServerMessageSize,16#09).


%% Well-known Parameter Assignments
%% Appendix A, Table 38, Page 106, WSP-Version-6-November-2000
-define(WSP_WELL_KNOWN_PARAMETERS,
	[
%%% Version 1.1
	 {16#00,1,'q'},
	 {16#01,1,'charset'},
	 {16#02,1,'level'},
	 {16#03,1,'type'},
	 {16#05,1,'name'},
	 {16#06,1,'filename'},
	 {16#07,1,'differences'},
	 {16#08,1,'padding'},
%%% Version 1.2
%% Note: From the Push specification
%%  Only used as parameter of Content-type multipart/related
	 {16#09,2,'type'},
	 {16#0A,2,'start'}, 
	 {16#0B,2,'start-info'}, 
%%% Version 1.3
	 {16#0C,3,'comment'},
	 {16#0D,3,'domain'},
	 {16#0E,3,'max-age'},
	 {16#0F,3,'path'},
	 {16#10,3,'secure'},
%%% Version 1.4
	 {16#11,4,'sec'},
	 {16#12,4,'mac'},
	 {16#13,4,'creation-date'},
	 {16#14,4,'modification-date'},
	 {16#15,4,'read-date'},
	 {16#16,4,'size'}
	]).
	
%% Header Field Name Assignments
%% Appendix A, Table 39, Page 107-108, WSP-Version-6-November-2000
%% Given as "Short-integer", with most significant bit set.
-define(WSP_WELL_KNOWN_PAGE1_HEADERS,
	[
%%% Version 1.1
	 {16#80,1,'accept'},
	 {16#81,1,'accept-charset'},
	 {16#82,1,'accept-encoding'},
	 {16#83,1,'accept-language'},
	 {16#84,1,'accept-ranges'},
	 {16#85,1,'age'},
	 {16#86,1,'allow'},
	 {16#87,1,'authorization'},
	 {16#88,1,'cache-control'},
	 {16#89,1,'connection'},
	 {16#8a,1,'content-base'}, % Excluded from HTTP RFC 2616
	 {16#8b,1,'content-encoding'},
	 {16#8c,1,'content-language'},
	 {16#8d,1,'content-length'},
	 {16#8e,1,'content-location'},
	 {16#8f,1,'content-md5'},
	 {16#90,1,'content-range'},
	 {16#91,1,'content-type'},
	 {16#92,1,'date'},
	 {16#93,1,'etag'},
	 {16#94,1,'expires'},
	 {16#95,1,'from'},
	 {16#96,1,'host'},
	 {16#97,1,'if-modified-since'},
	 {16#98,1,'if-match'},
	 {16#99,1,'if-none-match'},
	 {16#9a,1,'if-range'},
	 {16#9b,1,'if-unmodified-since'},
	 {16#9c,1,'location'},
	 {16#9d,1,'last-modified'},
	 {16#9e,1,'max-forwards'},
	 {16#9f,1,'pragma'},
	 {16#a0,1,'proxy-authenticate'},
	 {16#a1,1,'proxy-authorization'},
	 {16#a2,1,'public'}, % Excluded from HTTP RFC 2616
	 {16#a3,1,'range'},
	 {16#a4,1,'referer'},
	 {16#a5,1,'retry-after'},
	 {16#a6,1,'server'},
	 {16#a7,1,'transfer-encoding'},
	 {16#a8,1,'upgrade'},
	 {16#a9,1,'user-agent'},
	 {16#aa,1,'vary'},
	 {16#ab,1,'via'},
	 {16#ac,1,'warning'},
	 {16#ad,1,'www-authenticate'},
	 {16#ae,1,'content-disposition'}, % Proposed in HTTP
%%% Version 1.2
	 {16#af,2,'x-wap-application-id'}, % Used by Push 
	 {16#b0,2,'x-wap-content-uri'},    % Used by Push 
	 {16#b1,2,'x-wap-initiator-uri'},  % Used by Push 
	 {16#b2,2,'accept-application'},   % Used by Push 
	 {16#b3,2,'bearer-indication'},    % Used by Push 
	 {16#b4,2,'push-flag'},            % Used by Push 
	 {16#b5,2,'profile'},              % Used by UAProf 
	 {16#b6,2,'profile-diff'},         % Used by UAProf 
	 {16#b7,2,'profile-warning'},      % Used by UAProf 
%%% Version 1.3
	 {16#b8,3,'expect'},               % New in HTTP 1.1, RFC 2616
	 {16#b9,3,'te'},                   % New in HTTP 1.1, RFC 2616
	 {16#ba,3,'trailer'},              % New in HTTP 1.1, RFC 2616
	 {16#bb,3,'accept-charset'},       % Updated in HTTP 1.1, RFC 2616
	 {16#bc,3,'accept-encoding'},      % New in HTTP 1.1, RFC 2616
	 {16#bd,3,'cache-control'},        % Updated HTTP 1.1, RFC 2616
	 {16#be,3,'content-range'},        % New in HTTP 1.1, RFC 2616
	 {16#bf,3,'x-wap-tod'},            % Old WAP header...
	 {16#c0,3,'content-id'},           % From RFC 2045
	 {16#c1,3,'set-cookie'},           % Used by Cookie Mgmt
	 {16#c2,3,'cookie'},               % Used by Cookie Mgmt
	 {16#c3,3,'encoding-version'},     % Used by WSP
%%% Version 1.4
	 {16#c5,4,'profile-warning'},      % Updated encoding rule
	 {16#c6,4,'content-disposition'},  % Updated encoding rule
	 {16#c7,4,'x-wap-security'}        % Used by TL E2E Security
	]).

%% WAP specifics
-define(SHIFT_DELIMITER,		127).

%% Content Type Assignments, Appendix A, Table 40, Page 100-101
%% Given as "Short-integer", with most significant bit set.
-define(WSP_WELL_KNOWN_CONTENT_TYPES,
	[
%%% Version 1.1
	 {16#80,1,'*/*'},
	 {16#81,1,'text/*'},
	 {16#82,1,'text/html'},
	 {16#83,1,'text/plain'},
	 {16#84,1,'text/x-hdml'},
	 {16#85,1,'text/x-ttml'},
	 {16#86,1,'text/x-vcalendar'},
	 {16#87,1,'text/x-vcard'},
	 {16#88,1,'text/vnd.wap.wml'},
	 {16#89,1,'text/vnd.wap.wmlscript'},
	 {16#8a,1,'application/vnd.wap.channel'},
	 {16#8b,1,'multipart/*'},
	 {16#8c,1,'multipart/mixed'},
	 {16#8d,1,'multipart/form-data'},
	 {16#8e,1,'multipart/byteranges'},
	 {16#8f,1,'multipart/alternatives'},
	 {16#90,1,'application/*'},
	 {16#91,1,'application/java-vm'},
	 {16#92,1,'application/x-www-form-urlencoded'},
	 {16#93,1,'application/x-hdmlc'},
	 {16#94,1,'application/vnd.wap.wmlc'},
	 {16#95,1,'application/vnd.wap.wmlscriptc'},
	 {16#96,1,'application/vnd.wap.channelc'},
	 {16#97,1,'application/vnd.wap.uaprof'},
	 {16#98,1,'application/vnd.wap.wtls-ca-certificate'},
	 {16#99,1,'application/vnd.wap.wtls-user-certificate'},
	 {16#9a,1,'application/x-x509-ca-cert'},
	 {16#9b,1,'application/x-x509-user-cert'},
	 {16#9c,1,'image/*'},
	 {16#9d,1,'image/gif'},
	 {16#9e,1,'image/jpeg'},
	 {16#9f,1,'image/tiff'},
	 {16#a0,1,'image/png'},
	 {16#a1,1,'image/vnd.wap.wbmp'},
	 {16#a2,1,'application/vnd.wap.multipart.*'},
	 {16#a3,1,'application/vnd.wap.multipart.mixed'},
	 {16#a4,1,'application/vnd.wap.multipart.form-data'},
	 {16#a5,1,'application/vnd.wap.multipart.byteranges'},
	 {16#a6,1,'application/vnd.wap.multipart.alternative'},
	 {16#a7,1,'application/xml'},
	 {16#a8,1,'text/xml'},
	 {16#a9,1,'application/vnd.wap.wbxml'},
	 {16#aa,1,'application/x-x968-cross-cert'},
	 {16#ab,1,'application/x-x968-ca-cert'},
	 {16#ac,1,'application/x-x968-user-cert'},
%%% Version 1.2
	 {16#ad,2,'text/vnd.wap.si'},
	 {16#ae,2,'application/vnd.wap.sic'},
	 {16#af,2,'text/vnd.wap.sl'},
	 {16#b0,2,'application/vnd.wap.slc'},
	 {16#b1,2,'text/vnd.wap.co'},
	 {16#b2,2,'application/vnd.wap.coc'},
	 {16#b3,2,'application/vnd.wap.multipart.related'},
	 {16#b4,2,'application/vnd.wap.sia'},
%%% Version 1.3
	 {16#b5,3,'text/vnd.wap.connectivity-xml'},
	 {16#b6,3,'application/vnd.wap.connectivity-wbxml'}]).

%%% The subset of the above Content Types that are Multipart
-define(WSP_WELL_KNOWN_MULTIPART_CONTENT_TYPES,
	[
	 {16#8b,1,'multipart/*'},
	 {16#8c,1,'multipart/mixed'},
	 {16#8d,1,'multipart/form-data'},
	 {16#8e,1,'multipart/byteranges'},
	 {16#8f,1,'multipart/alternatives'},
	 {16#a2,1,'application/vnd.wap.multipart.*'},
	 {16#a3,1,'application/vnd.wap.multipart.mixed'},
	 {16#a4,1,'application/vnd.wap.multipart.form-data'},
	 {16#a5,1,'application/vnd.wap.multipart.byteranges'},
	 {16#a6,1,'application/vnd.wap.multipart.alternative'},
	 {16#b3,2,'application/vnd.wap.multipart.related'}]).

%% ISO 639 Language Assignments, Appendix A, Table 41, Page 102-103
-define(WSP_WELL_KNOWN_LANGUAGES,
	[
	 {16#01,['aa','afar']},
	 {16#02,['ab','abkhazian']},
	 {16#03,['af','afrikans']},
	 {16#04,['am','amharic']},
	 {16#05,['ar','arabic']},
	 {16#06,['as','assamese']},
	 {16#07,['ay','aymara']},
	 {16#08,['az','azerbaijani']},
	 {16#09,['ba','bashkir']},
	 {16#0a,['be','byelorussian']},
	 {16#0b,['bg','bulgarian']},
	 {16#0c,['bh','bihari']},
	 {16#0d,['bi','bislama']},
	 {16#0e,['ba','bangla','bengali']},
	 {16#0f,['bo','tibetan']},
	 {16#10,['br','breton']},
	 {16#11,['ca','catalan']},
	 {16#12,['co','corsican']},
	 {16#13,['cs','czech']},
	 {16#14,['cy','welsh']},
	 {16#15,['da','danish']},
	 {16#16,['de','german']},
	 {16#17,['dz','bhutani']},
	 {16#18,['el','greek']},
	 {16#19,['en','english']},
	 {16#1a,['eo','esperanto']},
	 {16#1b,['es','spanish']},
	 {16#1c,['et','estonian']},
	 {16#1d,['eu','basque']},
	 {16#1e,['fa','persian']},
	 {16#1f,['fi','finnish']},
	 {16#20,['fj','fiji']},
	 {16#82,['fo','faeroese']},
	 {16#22,['fr','french']},
	 {16#83,['fy','frisian']},
	 {16#24,['ga','irish']},
	 {16#25,['gd','scots-gaelic']},
	 {16#26,['gl','galician']},
	 {16#27,['ga','guarani']},
	 {16#28,['gu','gujarati']},
	 {16#29,['ha','hausa']},
	 {16#2a,['he','hebrew']},
	 {16#2b,['hi','hindi']},
	 {16#2c,['hr','croatian']},
	 {16#2d,['hu','hungarian']},
	 {16#2e,['hy','armenian']},
	 {16#84,['ia','interlingua']},
	 {16#30,['id','indonesian']},
	 {16#86,['ie','interlingue']},
	 {16#87,['ik','inupiak']},
	 {16#33,['is','icelandic']},
	 {16#34,['it','italian']},
	 {16#89,['iu','inuktitut']},
	 {16#36,['ja','japanese']},
	 {16#37,['jw','javanese']},
	 {16#38,['ka','georgian']},
	 {16#39,['kk','kazakh']},
	 {16#8a,['kl','greenlandic']},
	 {16#3b,['km','cambodian']},
	 {16#3c,['kn','kannada']},
	 {16#3d,['ko','korean']},
	 {16#3e,['ks','kashmiri']},
	 {16#3f,['ku','kurdish']},
	 {16#40,['ky','kirghiz']},
	 {16#8b,['la','latin']},
	 {16#42,['ln','lingala']},
	 {16#43,['lo','laothian']},
	 {16#44,['lt','lithuanian']},
	 {16#45,['lv','lettish','latvian']},
	 {16#46,['mg','malagese']},
	 {16#47,['mi','maori']},
	 {16#48,['mk','macedonian']},
	 {16#49,['ml','malayalam']},
	 {16#4a,['mn','mongolian']},
	 {16#4b,['mo','moldavian']},
	 {16#4c,['mr','marathi']},
	 {16#4d,['ms','malay']},
	 {16#4e,['mt','maltese']},
	 {16#4f,['my','burmese']},
	 {16#81,['na','nauru']},
	 {16#51,['ne','nepali']},
	 {16#52,['nl','dutch']},
	 {16#53,['no','norwegian']},
	 {16#54,['oc','occitan']},
	 {16#55,['om','oromo']},
	 {16#56,['or','oriya']},
	 {16#57,['pa','punjabi']},
	 {16#58,['po','polish']},
	 {16#59,['ps','pushto','pashto']},
	 {16#5a,['pt','portugese']},
	 {16#5b,['qu','quechua']},
	 {16#8c,['rm','rhaeto-romance']},
	 {16#5d,['rn','kirundi']},
	 {16#5e,['ro','romanian']},
	 {16#5f,['ru','russian']},
	 {16#60,['rw','kinyarwanda']},
	 {16#61,['sa','sanskrit']},
	 {16#62,['sd','sindhi']},
	 {16#63,['sg','sangho']},
	 {16#64,['sh','serbo-croatian']},
	 {16#65,['si','sinhalese']},
	 {16#66,['sk','slovak']},
	 {16#67,['sl','slovenian']},
	 {16#68,['sm','samoan']},
	 {16#69,['sn','shona']},
	 {16#6a,['so','somali']},
	 {16#6b,['sq','albanian']},
	 {16#6c,['sr','serbian']},
	 {16#6d,['ss','siswati']},
	 {16#6e,['st','seshoto']},
	 {16#6f,['su','sundanese']},
	 {16#70,['sv','swedish']},
	 {16#71,['sw','swahili']},
	 {16#72,['ta','tamil']},
	 {16#73,['te','telugu']},
	 {16#74,['tg','tajik']},
	 {16#75,['th','thai']},
	 {16#76,['ti','tigrinya']},
	 {16#77,['tk','turkmen']},
	 {16#78,['tl','tagalog']},
	 {16#79,['tn','setswana']},
	 {16#7a,['to','tonga']},
	 {16#7b,['tr','turkish']},
	 {16#7c,['ts','tsonga']},
	 {16#7d,['tt','tatar']},
	 {16#7e,['tw','twi']},
	 {16#7f,['ug','uighur']},
	 {16#50,['uk','ukrainian']},
	 {16#21,['ur','urdu']},
	 {16#23,['uz','uzbek']},
	 {16#2f,['vi','vietnamese']},
	 {16#85,['vo','volapuk']},
	 {16#31,['wo','wolof']},
	 {16#32,['xh','xhosa']},
	 {16#88,['yi','yiddish']},
	 {16#35,['yo','yoruba']},
	 {16#3a,['za','zhuang']},
	 {16#41,['zh','chinese']},
	 {16#5c,['zu','zulu']}]).

-define(ANY_LANGUAGE,128).
