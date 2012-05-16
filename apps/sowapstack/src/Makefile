#*******************************************************************
# $Id: Makefile,v 1.2 2001/07/09 12:27:22 johblo Exp $
#*******************************************************************

ERLC = erlc
ETAGS = etags

EBIN = ../ebin
GEN_FILES =

MODULES   = \
	wap_stack_sup wap_stack_man_sup \
	wap_common wap_stack_db \
	wap_stack wap_stack_man \
	wsp_pdu wsp_db \
	wsp_bytecodes wsp_bytecodes_headers wsp_headers_page1 \
	wsp_man \
	wsp_session_c wsp_method_c wsp_push_c \
	wsp_session_s wsp_method_s wsp_push_s \
	wtp_common wtp_pdu wtp_db \
	wtp_man wtp_initiator wtp_responder \
	wdp_man \
	base64encoder \
	sowapstack_log

HRL_FILES= ../include/wdpif.hrl ../include/wtpif.hrl ../include/wspif.hrl 

INTERNAL_HRL_FILES= wap_stack.hrl \
	wdp.hrl wtp.hrl wsp.hrl \
	wdp_bytecodes.hrl wtp_bytecodes.hrl wsp_bytecodes.hrl \
	stacklog.hrl

ERL_FILES= $(MODULES:%=%.erl)
APP_FILE = sowapstack.app
APP_SRC= $(APP_FILE).src

TARGET_FILES= $(MODULES:%=$(EBIN)/%.beam) $(APP_TARGET)
APP_TARGET= $(EBIN)/$(APP_FILE)

#------------------------------------------------------------
RM = /bin/rm -f

ERLFLAGS = 
ERL_COMPILE_FLAGS = -I../include

#------------------------------------------------------------
# Standard rules...

all: $(TARGET_FILES)

clean:
	$(RM) $(TARGET_FILES) $(GEN_FILES)

tags:
	$(ETAGS) $(ERL_SOURCES)


$(EBIN)/%.beam: %.erl
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(EBIN) $<

.erl.beam:
	$(ERLC) $(ERL_FLAGS) $(ERL_COMPILE_FLAGS) -o$(dir $@) $<

$(APP_TARGET): $(APP_SRC) 
	sed -e 's;%VSN%;$(SOWAPSTACK_RELEASE_VERSION);' $< > $@


#------------------------------------------------------------
# Other dependencies...
$(EBIN)/wsp_man.beam:		wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_session_c.beam:	wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_session_s.beam:	wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_method_c.beam:	wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_method_s.beam:	wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_push_c.beam:	wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_push_s.beam:	wsp.hrl ../include/wspif.hrl
$(EBIN)/wsp_db.beam:		wsp.hrl
$(EBIN)/wsp_pdu.beam:		wsp.hrl
$(EBIN)/wsp_bytecodes.beam:	wsp_bytecodes.hrl
$(EBIN)/wtp_man.beam:		wtp.hrl ../include/wtpif.hrl
$(EBIN)/wtp_initiator.beam:	wtp.hrl ../include/wtpif.hrl
$(EBIN)/wtp_responder.beam:	wtp.hrl ../include/wtpif.hrl
$(EBIN)/wtp_common.beam:	wtp.hrl
$(EBIN)/wtp_db.beam:		wtp.hrl
$(EBIN)/wdp_man.beam:		wdp.hrl ../include/wdpif.hrl
$(EBIN)/wap_stack_db.beam:	wdp.hrl


#------------------------------------------------------------
# Testing...
