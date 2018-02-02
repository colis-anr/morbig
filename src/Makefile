# -*- makefile -*-
.PHONY: all clean debug doc .headers

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

OCAMLBUILD = ocamlbuild -use-ocamlfind $(OCAMLBUILDFLAGS)

all:
	echo "let current=\"`cat VERSION`\"" > version.ml
	echo "let timestamp=\"`date +%Y%m%d`\"" >> version.ml
	echo "let commit = \"`git log -1 --pretty=format:%H`\"" >> version.ml
	$(OCAMLBUILD) morbig.native libmorbig.cma libmorbig.cmxa libmorbig.cmxs

debug:
	$(OCAMLBUILD) -tag debug morbig.byte libmorbig.cma

clean:
	$(OCAMLBUILD) -clean

doc:
	$(OCAMLBUILD) libmorbig.docdir/index.html

headers:
	headache -h .header *.ml *.mli *.mll *.mly

