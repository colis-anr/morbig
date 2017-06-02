.PHONY: all clean debug

OCAMLBUILD=ocamlbuild -use-ocamlfind

# Menhir can be told to produce a parser that explains what
# it is doing.
MENHIROPT_COMMON=\
   -yaccflag --explain -yaccflag --inspection -yaccflag --table \
   -yaccflag --dump

ifeq ($(DEBUGPARSING), yes)
  MENHIROPT=$(MENHIROPT_COMMON) -yaccflag --trace
else
  MENHIROPT=$(MENHIROPT_COMMON)
endif

OCAMLBUILDFLAGS_COMMON = $(MENHIROPT)

# In Emacs, use classic display to enable error jumping.
TERM = $(shell echo $$TERM)
ifeq ($(TERM), dumb)
 OCAMLBUILDFLAGS = $(OCAMLBUILDFLAGS_COMMON) -tag debug -classic-display
else
 OCAMLBUILDFLAGS = $(OCAMLBUILDFLAGS_COMMON)
endif

OCAMLBUILD = ocamlbuild -use-ocamlfind -use-menhir $(OCAMLBUILDFLAGS)

all:
	$(OCAMLBUILD) morbigparser.native

dump:
	$(OCAMLBUILD) shdump.native

test:
	$(OCAMLBUILD) -lib unix shtest.native
	$(OCAMLBUILD) shjson.native

prelex.native: CST.ml prelexer.mll prelexer.mli parser.mly prelex.ml
	$(OCAMLBUILD) $@

debug:
	$(OCAMLBUILD) -tag debug shparser.byte shstats.byte

clean:
	$(OCAMLBUILD) -clean
