TESTS = test001 

TESTS_ENVIRONMENT = EXEEXT=$(EXEEXT) $(srcdir)/test.sh

include $(top_srcdir)/m4/Makefile.am.vars

#PARSER_INCLUDES += -I $(top_srcdir)/src
MODULE_INCLUDES += -I $(top_srcdir)/src

EXTRA_CMXA += $(PACKAGE).cmxa

# OCaml parser call with flags
AM_P5FLAGS := -pp "$(CAMLP5O) $(PARSER_INCLUDES) pa_log.cmo $(LOG)"

# overwrite some variables
PXFLAGS = $(P5FLAGS)
CAMLPXLIB = $(CAMLP5LIB)
MODULE_INCLUDES += -I $(CAMLP5LIB)

TARGET = 

CLEAN_EXTRA += $(addsuffix .cmi, $(TESTS)) $(addsuffix .cmo, $(TESTS)) $(addsuffix .cmx, $(TESTS)) $(addsuffix .o, $(TESTS))
CLEAN_EXTRA += $(addsuffix $(EXEEXT), $(TESTS))
CLEAN_EXTRA += *.diff *.log

include $(top_srcdir)/m4/Makefile.am.targets

# Rules to build tests
%: %.ml
	$(OCAMLOPT) $(BFLAGS) $(EXTRA_CMXA) $(PXFLAGS) -o $@$(EXEEXT) $<

include $(top_srcdir)/m4/Makefile.am.rules
