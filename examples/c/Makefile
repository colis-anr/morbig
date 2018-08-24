.PHONY: all clean

STDDIR=$(shell ocamlfind ocamlc -where)
LIBDIR=$(shell ocamlfind query libmorbig)

all:
	$(CC) -o dump -I $(STDDIR) -I $(LIBDIR) \
            dump.c -lm -ldl $(LIBDIR)/libmorbigc.a

clean:
	rm -f dump
