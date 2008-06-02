ERL=$(shell which erl)
ERLC=$(shell which erlc)

EMULATOR=beam

ifdef debug
  ERLC_FLAGS+=-Ddebug
endif

ifdef trace
  ERLC_FLAGS=+trace
endif

ifdef export_all
  ERLC_FLAGS+=-Dexport_all
endif

INSTALL=install -c
INSTALL_DATA=${INSTALL} -m 644

../ebin/%.$(EMULATOR): %.erl
	$(ERLC) -b $(EMULATOR) $(ERLC_FLAGS) -o ../ebin $<

%.$(EMULATOR): %.erl
	$(ERLC) -b $(EMULATOR) $(ERLC_FLAGS) $<


ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

MODULES=test_client test_udp_client test_tcp_client test_udp test_tcp


EBIN_FILES=$(MODULES:%=../ebin/%.$(EMULATOR)) 

ERLC_FLAGS+=-W +debug_info $(DEBUG_FLAGS) -I../include -pa ../ebin

#
# Targets
#

all: $(EBIN_FILES) 

debug:
	$(MAKE) TYPE=debug

clean:
	rm -f $(EBIN_FILES)

install: all
	install -d ../ebin
	(cd ../ebin; find . -name '*' -print | xargs tar cf - ) | (cd ../ebin; tar xf - )

tags:
	erl  -noshell -pa ../ebin -s mail_debug mktags

xref:
	erl -noshell -pa ../ebin -s mail_debug xref ../ebin

