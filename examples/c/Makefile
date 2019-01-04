.PHONY: all clean

STDDIR=$(shell ocamlfind ocamlc -where)
LIBDIR=$(shell ocamlfind query morbig)

all:
	$(CC) -o dump -I $(STDDIR) -I $(LIBDIR) \
            dump.c $(LIBDIR)/libmorbigc.a -lm -ldl

clean:
	rm -f dump
