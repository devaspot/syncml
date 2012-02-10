
% PAGE 1
-define(SYNCML_META_anchor,     16#05).
-define(SYNCML_META_emi,        16#06).
-define(SYNCML_META_format,     16#07).
-define(SYNCML_META_freeid,     16#08).
-define(SYNCML_META_freemem,    16#09).
-define(SYNCML_META_last,       16#0A).
-define(SYNCML_META_mark,       16#0B).
-define(SYNCML_META_maxmsgsize, 16#0C).
-define(SYNCML_META_mem,        16#0D).
-define(SYNCML_META_metinf,     16#0E).
-define(SYNCML_META_next,       16#0F).
-define(SYNCML_META_nextnonce,  16#10).
-define(SYNCML_META_sharedmem,  16#11).
-define(SYNCML_META_size,       16#12).
-define(SYNCML_META_type,       16#13).
-define(SYNCML_META_version,    16#14).
-define(SYNCML_META_maxobjsize, 16#15).

% PAGE 0
-define(SYNCML_tag1,    16#01).
-define(SYNCML_tag2,    16#02).
-define(SYNCML_tag3,    16#03).
-define(SYNCML_tag4,    16#04).
-define(SYNCML_tag5,    16#05).
-define(SYNCML_tag6,    16#06).
-define(SYNCML_tag7,    16#07).
-define(SYNCML_tag8,    16#08).
-define(SYNCML_tag9,    16#09).
-define(SYNCML_tagA,    16#0A).
-define(SYNCML_tagB,    16#0b).
-define(SYNCML_tagC,    16#0c).
-define(SYNCML_tagD,    16#0d).
-define(SYNCML_tagE,    16#0e).
-define(SYNCML_tagF,    16#0f).
-define(SYNCML_tag10,   16#10).
-define(SYNCML_tag11,   16#11).
-define(SYNCML_tag12,   16#12).
-define(SYNCML_tag13,   16#13).
-define(SYNCML_tag14,   16#14).
-define(SYNCML_tag15,   16#15).
-define(SYNCML_tag16,   16#16).
-define(SYNCML_tag17,   16#17).
-define(SYNCML_tag18,   16#18).
-define(SYNCML_tag19,   16#19).
-define(SYNCML_meta,        16#1A).
-define(SYNCML_msgid,       16#1B).
-define(SYNCML_msgref,      16#1c).
-define(SYNCML_noresp,      16#1d).
-define(SYNCML_noresults,   16#1e).
-define(SYNCML_put,     16#1f).
-define(SYNCML_replace,     16#20).
-define(SYNCML_respuri,     16#21).
-define(SYNCML_results,     16#22).
-define(SYNCML_search,      16#23).
-define(SYNCML_sequence,    16#24).
-define(SYNCML_sessionid,   16#25).
-define(SYNCML_sftdel,      16#26).
-define(SYNCML_source,      16#27).
-define(SYNCML_sourceref,   16#28).
-define(SYNCML_status,      16#29).
-define(SYNCML_sync,        16#2a).
-define(SYNCML_syncbody,    16#2b).
-define(SYNCML_synchdr,     16#2c).
-define(SYNCML_syncml,      16#2d).
-define(SYNCML_target,      16#2e).
-define(SYNCML_targetref,   16#2f).
-define(SYNCML_tag30,       16#30).
-define(SYNCML_verdtd,      16#31).
-define(SYNCML_verproto,    16#32).
-define(SYNCML_numberofchanges, 16#33).
-define(SYNCML_moredata,    16#34).
-define(SYNCML_tag35,       16#35).
-define(SYNCML_tag36,       16#36).
-define(SYNCML_tag37,       16#37).
-define(SYNCML_tag38,       16#38).
-define(SYNCML_tag39,       16#39).
-define(SYNCML_tag3A,       16#3A).
-define(SYNCML_tag3B,       16#3b).
-define(SYNCML_tag3C,       16#3c).
-define(SYNCML_tag3D,       16#3d).
-define(SYNCML_tag3E,       16#3E).
-define(SYNCML_tag3F,       16#3f).


-define(SYNCML_TAG_TOKENS,
    [[
     {?SYNCML_tag1,"tag1"},
     {?SYNCML_tag2,"tag2"},
     {?SYNCML_tag3,"tag3"},
     {?SYNCML_tag4,"tag4"},
     {?SYNCML_tag5,"Add"},
     {?SYNCML_tag6,"Alert"},
     {?SYNCML_tag7,"Archive"},
     {?SYNCML_tag8,"Atomic"},
     {?SYNCML_tag9,"Chal"},
     {?SYNCML_tagA,"Cmd"},
     {?SYNCML_tagB,"CmdID"},
     {?SYNCML_tagC,"CmdRef"},
     {?SYNCML_tagD,"Copy"},
     {?SYNCML_tagE,"Cred"},
     {?SYNCML_tagF,"Data"},
     {?SYNCML_tag10,"Delete"},
     {?SYNCML_tag11,"Exec"},
     {?SYNCML_tag12,"Final"},
     {?SYNCML_tag13,"Get"},
     {?SYNCML_tag14,"Item"},
     {?SYNCML_tag15,"Lang"},
     {?SYNCML_tag16,"LocName"},
     {?SYNCML_tag17,"LocURI"},
     {?SYNCML_tag18,"Map"},
     {?SYNCML_tag19,"MapItem"},
     {?SYNCML_meta,"Meta"},
     {?SYNCML_msgid,"MsgID"},
     {?SYNCML_msgref,"MsgRef"},
     {?SYNCML_noresp,"NoResp"},
     {?SYNCML_noresults,"NoResults"},
     {?SYNCML_put,"Put"},
     {?SYNCML_replace,"Replace"},
     {?SYNCML_respuri,"RespURI"},
     {?SYNCML_results,"Results"},
     {?SYNCML_search,"Search"},
     {?SYNCML_sequence,"Sequence"},
     {?SYNCML_sessionid,"SessionID"},
     {?SYNCML_sftdel,"SftDel"},
     {?SYNCML_source,"Source"},
     {?SYNCML_sourceref,"SourceRef"},
     {?SYNCML_status,"Status"},
     {?SYNCML_sync,"Sync"},
     {?SYNCML_syncbody,"SyncBody"},
     {?SYNCML_synchdr,"SyncHdr"},
     {?SYNCML_syncml,"SyncML"},
     {?SYNCML_target,"Target"},
     {?SYNCML_targetref,"TargetRef"},
     {?SYNCML_verdtd,"VerDTD"},
     {?SYNCML_verproto,"VerProto"},
     {?SYNCML_numberofchanges,"NumberOfChanges"},
     {?SYNCML_moredata,"MoreData"}],
    [
    {?SYNCML_META_anchor,   "Anchor"},
    {?SYNCML_META_emi,  "EMI"},
    {?SYNCML_META_format,   "Format"},
    {?SYNCML_META_freeid,   "FreeID"},
    {?SYNCML_META_freemem,  "FreeMem"},
    {?SYNCML_META_last,     "Last"},
    {?SYNCML_META_mark,     "Mark"},
    {?SYNCML_META_maxmsgsize, "MaxMsgSize"},
    {?SYNCML_META_mem,  "Mem"},
    {?SYNCML_META_metinf,   "MetInf"},
    {?SYNCML_META_next,     "Next"},
    {?SYNCML_META_nextnonce, "NextNonce"},
    {?SYNCML_META_sharedmem, "SharedMem"},
    {?SYNCML_META_size,     "Size"},
    {?SYNCML_META_type,     "Type"},
    {?SYNCML_META_version,  "Version"},
    {?SYNCML_META_maxobjsize, "MaxObjSize"}]]).

