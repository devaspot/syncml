
%% Tag Tokens
-define(WML_tag1,	16#01).
-define(WML_tag2,	16#02).
-define(WML_tag3,	16#03).
-define(WML_tag4,	16#04).
-define(WML_tag5,	16#05).
-define(WML_tag6,	16#06).
-define(WML_tag7,	16#07).
-define(WML_tag8,	16#08).
-define(WML_tag9,	16#09).
-define(WML_tagA,	16#0A).
-define(WML_tagB,	16#0b).
-define(WML_tagC,	16#0c).
-define(WML_tagD,	16#0d).
-define(WML_tagE,	16#0e).
-define(WML_tagF,	16#0f).
-define(WML_tag10,	16#10).
-define(WML_tag11,	16#11).
-define(WML_tag12,	16#12).
-define(WML_tag13,	16#13).
-define(WML_tag14,	16#14).
-define(WML_tag15,	16#15).
-define(WML_tag16,	16#16).
-define(WML_tag17,	16#17).
-define(WML_tag18,	16#18).
-define(WML_tag19,	16#19).
-define(WML_tag1A,	16#1A).
-define(WML_tag1B,	16#1B).
-define(WML_a,		16#1c).
-define(WML_td,		16#1d).
-define(WML_tr,		16#1e).
-define(WML_table,	16#1f).
-define(WML_p,		16#20).
-define(WML_postfield,	16#21).
-define(WML_anchor,	16#22).
-define(WML_access,	16#23).
-define(WML_b,		16#24).
-define(WML_big,	16#25).
-define(WML_br,		16#26).
-define(WML_card,	16#27).
-define(WML_do,		16#28).
-define(WML_em,		16#29).
-define(WML_fieldset,	16#2a).
-define(WML_go,		16#2b).
-define(WML_head,	16#2c).
-define(WML_i,		16#2d).
-define(WML_img,	16#2e).
-define(WML_input,	16#2f).
-define(WML_meta,	16#30).
-define(WML_noop,	16#31).
-define(WML_prev,	16#32).
-define(WML_onevent,	16#33).
-define(WML_optgroup,	16#34).
-define(WML_option,	16#35).
-define(WML_refresh,	16#36).
-define(WML_select,	16#37).
-define(WML_setvar,	16#3e).
-define(WML_small,	16#38).
-define(WML_strong,	16#39).
-define(WML_tag3A,	16#3A).
-define(WML_template,	16#3b).
-define(WML_timer,	16#3c).
-define(WML_u,		16#3d).
-define(WML_tag3E,	16#3E).
-define(WML_wml,	16#3f).


-define(WML_TAG_TOKENS,
	[
	 {?WML_tag1,"tag1"},
	 {?WML_tag2,"tag2"},
	 {?WML_tag3,"tag3"},
	 {?WML_tag4,"tag4"},
	 {?WML_tag5,"tag5"},
	 {?WML_tag6,"tag6"},
	 {?WML_tag7,"tag7"},
	 {?WML_tag8,"tag8"},
	 {?WML_tag9,"tag9"},
	 {?WML_tagA,"tagA"},
	 {?WML_tagB,"tagB"},
	 {?WML_tagC,"tagC"},
	 {?WML_tagD,"tagD"},
	 {?WML_tagE,"tagE"},
	 {?WML_tagF,"tagF"},
	 {?WML_tag10,"tag10"},
	 {?WML_tag11,"tag11"},
	 {?WML_tag12,"tag12"},
	 {?WML_tag13,"tag13"},
	 {?WML_tag14,"tag14"},
	 {?WML_tag15,"tag15"},
	 {?WML_tag16,"tag16"},
	 {?WML_tag17,"tag17"},
	 {?WML_tag18,"tag18"},
	 {?WML_tag19,"tag19"},
	 {?WML_tag1A,"tag1A"},
	 {?WML_tag1B,"tag1B"},
	 {?WML_a,"a"},
	 {?WML_anchor,"anchor"},
	 {?WML_access,"access"},
	 {?WML_b,"b"},
	 {?WML_big,"big"},
	 {?WML_br,"br"},
	 {?WML_card,"card"},
	 {?WML_do,"do"},
	 {?WML_em,"em"},
	 {?WML_fieldset,"fieldset"},
	 {?WML_go,"go"},
	 {?WML_head,"head"},
	 {?WML_i,"i"},
	 {?WML_img,"img"},
	 {?WML_input,"input"},
	 {?WML_meta,"meta"},
	 {?WML_noop,"noop"},
	 {?WML_p,"p"},
	 {?WML_postfield,"postfield"},
	 {?WML_prev,"prev"},
	 {?WML_onevent,"onevent"},
	 {?WML_optgroup,"optgroup"},
	 {?WML_option,"option"},
	 {?WML_refresh,"refresh"},
	 {?WML_select,"select"},
	 {?WML_setvar,"setvar"},
	 {?WML_small,"small"},
	 {?WML_strong,"strong"},
	 {?WML_table,"table"},
	 {?WML_td,"td"},
	 {?WML_template,"template"},
	 {?WML_timer,"timer"},
	 {?WML_tr,"tr"},
	 {?WML_u,"u"},
	 {?WML_wml,"wml"}]).


%% Attribute Start Tokens and Attribute Value Tokens
%% Section 14.3.3, Table 6, Page 67-68, WML-Version-16-Jun-1999
%% Section 14.3.4, Table 7, Page 68-69, WML-Version-16-Jun-1999
-define(WML_acceptcharset, 	16#05).
-define(WML_align,	   	16#52).
-define(WML_align_bottom,  	16#06).
-define(WML_align_center,  	16#07).
-define(WML_align_left,	   	16#08).
-define(WML_align_middle,  	16#09).
-define(WML_align_right,   	16#0a).
-define(WML_align_top,	   	16#0b).
-define(WML_alt,	   	16#0c).
-define(WML_class,	   	16#54).
-define(WML_columns,	   	16#53).
-define(WML_content,	   	16#0d).
-define(WML_content_application_vndwapwmlc,16#5c).
-define(WML_domain,	   	16#0f).
-define(WML_emptyok_false, 	16#10).
-define(WML_emptyok_true,  	16#11).
-define(WML_format,	   	16#12).
-define(WML_forua_false,   	16#56).
-define(WML_forua_true,	   	16#57).
-define(WML_height,	   	16#13).
-define(WML_href,	   	16#4a).
-define(WML_href_http,	   	16#4b).
-define(WML_href_https,	   	16#4c).
-define(WML_hspace,	   	16#14).
-define(WML_httpequiv,	   	16#5a).
-define(WML_httpequiv_ContentType,16#5b).
-define(WML_httpequiv_Expires,	16#5d).
-define(WML_id,		   	16#55).
-define(WML_ivalue,	   	16#15).
-define(WML_iname,	   	16#16).
-define(WML_label,	   	16#18).
-define(WML_localsrc,	   	16#19).
-define(WML_maxlength,	   	16#1a).
-define(WML_method_get,	   	16#1b).
-define(WML_method_post,   	16#1c).
-define(WML_mode_nowrap,   	16#1d).
-define(WML_mode_wrap,	   	16#1e).
-define(WML_multiple_false,	16#1f).
-define(WML_multiple_true, 	16#20).
-define(WML_name,	   	16#21).
-define(WML_newcontext_false,	16#22).
-define(WML_newcontext_true,	16#23).
-define(WML_onenterbackward,	16#25).
-define(WML_onenterforward,	16#26).
-define(WML_onpick,	   	16#24).
-define(WML_ontimer,	   	16#27).
-define(WML_optional_false,	16#28).
-define(WML_optional_true, 	16#29).
-define(WML_path,	   	16#2a).
-define(WML_scheme,	   	16#2e).
-define(WML_sendreferer_false,	16#2f).
-define(WML_sendreferer_true,	16#30).
-define(WML_size,	   	16#31).
-define(WML_src,	   	16#32).
-define(WML_src_http,	   	16#58).
-define(WML_src_https,	   	16#59).
-define(WML_ordered_true,  	16#33).
-define(WML_ordered_false, 	16#34).
-define(WML_tabindex,	   	16#35).
-define(WML_title,	   	16#36).
-define(WML_type,	   	16#37).
-define(WML_type_accept,   	16#38).
-define(WML_type_delete,   	16#39).
-define(WML_type_help,	   	16#3a).
-define(WML_type_password, 	16#3b).
-define(WML_type_onpick,   	16#3c).
-define(WML_type_onenterbackward,16#3d).
-define(WML_type_onenterforward,16#3e).
-define(WML_type_ontimer,  	16#3f).
-define(WML_type_options,  	16#45).
-define(WML_type_prev,	   	16#46).
-define(WML_type_reset,	   	16#47).
-define(WML_type_text,	   	16#48).
-define(WML_type_vnd,	   	16#49).
-define(WML_value,	   	16#4d).
-define(WML_vspace,	   	16#4e).
-define(WML_width,	   	16#4f).
-define(WML_xmllang,	   	16#50).

-define(WML_ATTRIBUTE_START_TOKENS_DEC,
	[
	 {?WML_acceptcharset,	"accept-charset=\""},
	 {?WML_align,		"align=\""},
	 {?WML_align_bottom,	"align=\"bottom\""},
	 {?WML_align_center,	"align=\"center\""},
	 {?WML_align_left,	"align=\"left\""},
	 {?WML_align_middle,	"align=\"middle\""},
	 {?WML_align_right,	"align=\"right\""},
	 {?WML_align_top,	"align=\"top\""},
	 {?WML_alt,		"alt=\""},
	 {?WML_class,		"class=\""},
	 {?WML_columns,		"columns=\""},
	 {?WML_content,		"content=\""},
	 {?WML_content_application_vndwapwmlc,"content=\"application/vnd.wap.wmlc;charset\""},
	 {?WML_domain,		"domain=\""},
	 {?WML_emptyok_false,	"emptyok=\"false\""},
	 {?WML_emptyok_true,	"emptyok=\"true\""},
	 {?WML_format,		"format=\""},
	 {?WML_forua_false,	"forua=\"false\""},
	 {?WML_forua_true,	"forua=\"true\""},
	 {?WML_height,		"height=\""},
	 {?WML_href,		"href=\""},
	 {?WML_href_http,	"href=\"http://"},
	 {?WML_href_https,	"href=\"https://"},
	 {?WML_hspace,		"hspace=\""},
	 {?WML_httpequiv,	"http-equiv=\""},
	 {?WML_httpequiv_ContentType,"http-equiv=\"Content-Type\""},
	 {?WML_httpequiv_Expires,"http-equiv=\"Expires\""},
	 {?WML_id,		"id=\""},
	 {?WML_ivalue,		"ivalue=\""},
	 {?WML_iname,		"iname=\""},
	 {?WML_label,		"label=\""},
	 {?WML_localsrc,	"localsrc=\""},
	 {?WML_maxlength,	"maxlength=\""},
	 {?WML_method_get,	"method=\"get\""},
	 {?WML_method_post,	"method=\"post\""},
	 {?WML_mode_nowrap,	"mode=\"nowrap\""},
	 {?WML_mode_wrap,	"mode=\"wrap\""},
	 {?WML_multiple_false,	"multiple=\"false\""},
	 {?WML_multiple_true,	"multiple=\"true\""},
	 {?WML_name,		"name"},
	 {?WML_newcontext_false,"newcontext=\"false\""},
	 {?WML_newcontext_true,	"newcontext=\"true\""},
	 {?WML_onenterbackward,	"onenterbackward"},
	 {?WML_onenterforward,	"onenterforward"},
	 {?WML_onpick,		"onpick"},
	 {?WML_ontimer,		"ontimer"},
	 {?WML_optional_false,	"optional=\"false\""},
	 {?WML_optional_true,	"optional=\"true\""},
	 {?WML_path,		"path=\""},
	 {?WML_scheme,		"scheme=\""},
	 {?WML_sendreferer_false,"sendreferer=\"false\""},
	 {?WML_sendreferer_true,"sendreferer=\"true\""},
	 {?WML_size,		"size=\""},
	 {?WML_src,		"src=\""},
	 {?WML_src_http,	"src=\"http://"},
	 {?WML_src_https,	"src=\"https://"},
	 {?WML_ordered_true,	"ordered=\"true\""},
	 {?WML_ordered_false,	"ordered=\"false\""},
	 {?WML_tabindex,	"tabindex=\""},
	 {?WML_title,		"title=\""},
	 {?WML_type,		"type=\""},
	 {?WML_type_accept,	"type=\"accept\""},
	 {?WML_type_delete,	"type=\"delete\""},
	 {?WML_type_help,	"type=\"help\""},
	 {?WML_type_password,	"type=\"password\""},
	 {?WML_type_onpick,	"type=\"onpick"},
	 {?WML_type_onenterbackward,"type=\"onenterbackward\""},
	 {?WML_type_onenterforward,"type=\"onenterforward\""},
	 {?WML_type_ontimer,	"type=\"ontimer\""},
	 {?WML_type_options,	"type=\"options\""},
	 {?WML_type_prev,	"type=\"prev\""},
	 {?WML_type_reset,	"type=\"reset\""},
	 {?WML_type_text,	"type=\"text\""},
	 {?WML_type_vnd,	"type=\"vnd."},
	 {?WML_value,		"value=\""},
	 {?WML_vspace,		"vspace=\""},
	 {?WML_width,		"width=\""},
	 {?WML_xmllang,		"xml:lang=\""}
	]).

-define(WML_ATTRIBUTE_START_TOKENS_ENC,
	[
	 {'accept-charset',?WML_acceptcharset},
	 {'align',align},
	 {'alt',?WML_alt},
	 {'class',?WML_class},
	 {'columns',?WML_columns},
	 {'content',content},
	 {'domain',?WML_domain},
	 {'emptyok',emptyok},
	 {'format',?WML_format},
	 {'forua',forua},
	 {'height',?WML_height},
	 {'href',href},
	 {'hspace',?WML_hspace},
	 {'http-equiv','http-equiv'},
	 {'id',?WML_id},
	 {'ivalue',?WML_ivalue},
	 {'iname',?WML_iname},
	 {'label',?WML_label},
	 {'localsrc',?WML_localsrc},
	 {'maxlength',?WML_maxlength},
	 {'method',method},
	 {'mode',mode},
	 {'multiple',multiple},
	 {'name',?WML_name},
	 {'newcontext',newcontext},
	 {'onenterbackward',?WML_onenterbackward},
	 {'onenterforward',?WML_onenterforward},
	 {'onpick',?WML_onpick},
	 {'ontimer',?WML_ontimer},
	 {'optional',optional},
	 {'path',?WML_path},
	 {'scheme',?WML_scheme},
	 {'sendreferer',sendreferer},
	 {'size',?WML_size},
	 {'src',src},
	 {'ordered',ordered},
	 {'tabindex',?WML_tabindex},
	 {'title',?WML_title},
	 {'type',type},
	 {'value',?WML_value},
	 {'vspace',?WML_vspace},
	 {'width',?WML_width},
	 {'xml:lang',?WML_xmllang}
	]).


-define(WML_ATTVAL_com,16#85).
-define(WML_ATTVAL_edu,16#86).
-define(WML_ATTVAL_net,16#87).
-define(WML_ATTVAL_org,16#88).
-define(WML_ATTVAL_accept,16#89).
-define(WML_ATTVAL_bottom,16#8a).
-define(WML_ATTVAL_clear,16#8b).
-define(WML_ATTVAL_delete,16#8c).
-define(WML_ATTVAL_help,16#8d).
-define(WML_ATTVAL_http,16#8e).
-define(WML_ATTVAL_http_www,16#8f).
-define(WML_ATTVAL_https,16#90).
-define(WML_ATTVAL_https_www,16#91).
-define(WML_ATTVAL_middle,16#93).
-define(WML_ATTVAL_nowrap,16#94).
-define(WML_ATTVAL_onenterbackward,16#96).
-define(WML_ATTVAL_onenterforward,16#97).
-define(WML_ATTVAL_onpick,16#95).
-define(WML_ATTVAL_ontimer,16#98).
-define(WML_ATTVAL_options,16#99).
-define(WML_ATTVAL_password,16#9a).
-define(WML_ATTVAL_reset,16#9b).
-define(WML_ATTVAL_text,16#9d).
-define(WML_ATTVAL_top,16#9e).
-define(WML_ATTVAL_unknown,16#9f).
-define(WML_ATTVAL_wrap,16#a0).
-define(WML_ATTVAL_www,16#a1).


-define(WML_ATTRIBUTE_VALUE_TOKENS,
	[
	 {?WML_ATTVAL_com,".com/"},
	 {?WML_ATTVAL_edu,".edu/"},
	 {?WML_ATTVAL_net,".net/"},
	 {?WML_ATTVAL_org,".org/"},
	 {?WML_ATTVAL_accept,"accept"},
	 {?WML_ATTVAL_bottom,"bottom"},
	 {?WML_ATTVAL_clear,"clear"},
	 {?WML_ATTVAL_delete,"delete"},
	 {?WML_ATTVAL_help,"help"},
	 {?WML_ATTVAL_http,"http://"},
	 {?WML_ATTVAL_http_www,"http://www."},
	 {?WML_ATTVAL_https,"https://"},
	 {?WML_ATTVAL_https_www,"https://www."},
	 {?WML_ATTVAL_middle,"middle"},
	 {?WML_ATTVAL_nowrap,"nowrap"},
	 {?WML_ATTVAL_onenterbackward,"onenterbackward"},
	 {?WML_ATTVAL_onenterforward,"onenterforward"},
	 {?WML_ATTVAL_onpick,"onpick"},
	 {?WML_ATTVAL_ontimer,"ontimer"},
	 {?WML_ATTVAL_options,"options"},
	 {?WML_ATTVAL_password,"password"},
	 {?WML_ATTVAL_reset,"reset"},
	 {?WML_ATTVAL_text,"text"},
	 {?WML_ATTVAL_top,"top"},
	 {?WML_ATTVAL_unknown,"unknown"},
	 {?WML_ATTVAL_wrap,"wrap"},
	 {?WML_ATTVAL_www,"www."}
	]).

