.PHONY: all clean

STDDIR=$(shell ocamlfind ocamlc -where)
LIBDIR=$(shell ocamlfind query libmorbig)

all:
	$(CC) -o dump -I $(STDDIR) -I $(LIBDIR) \
            dump.c -lm -ldl `ocamlc -where`/libcamlrun.a $(LIBDIR)/libmorbigc.a $(LIBDIR)/libmorbig.a

clean:
	rm dump
