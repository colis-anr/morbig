.PHONY: all clean

STDDIR=$(shell ocamlfind ocamlc -where)
LIBDIR=$(shell ocamlfind query morbig)

all:
	$(CC) -o dump -I $(STDDIR) -I $(LIBDIR) \
            dump.c $(LIBDIR)/libmorbigc.o -lm -ldl -lpthreads

clean:
	rm -f dump
