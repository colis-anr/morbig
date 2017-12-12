.PHONY: all debug clean tests uninstall doc

EXPORTED_SOURCES=				\
	src/CST.ml				\
	src/API.mli

all:
	$(MAKE) -C src
	mkdir -p bin lib
	cp src/morbig.native bin/morbig
	cp src/_build/libmorbig.o src/_build/libmorbig.cm* src/_build/libmorbig.a lib

doc:
	$(MAKE) -C src doc
	cp -fr src/libmorbig.docdir/* doc

debug:
	DEBUGPARSING=yes $(MAKE) -C src debug
	mkdir -p bin
	cp src/morbig.byte bin/morbig

install:
	@ if [ x$(PREFIX) = x ]; then								\
	  echo "Selecting OPAM based install."; 						\
	  echo "Specify PREFIX=... for system-wide install.";					\
	  cp bin/morbig `opam config var bin`;							\
	  cp man/morbig.1 `opam config var man`;						\
	  mkdir -p `opam config var doc`/libmorbig;						\
	  cp -fr doc/* `opam config var doc`/libmorbig;						\
	  ocamlfind remove libmorbig;								\
	  ocamlfind install libmorbig META lib/* $(EXPORTED_SOURCES);				\
	else											\
	  echo "Selecting system-wide install. PREFIX is $(PREFIX)";				\
	  cp bin/morbig $(PREFIX)/bin;								\
	  mkdir -p $(PREFIX)/share/man/man1;							\
	  cp man/morbig.1 $(PREFIX)/share/man/man1;						\
	  mkdir -p $(PREFIX)/share/doc/libmorbig;						\
	  cp -fr doc/* $(PREFIX)/share/doc/libmorbig;						\
	  ocamlfind remove -destdir $(PREFIX)/lib libmorbig;					\
	  ocamlfind install -destdir $(PREFIX)/lib libmorbig META $(EXPORTED_SOURCES) lib/*;	\
         fi

uninstall:
	@ if [ x$(PREFIX) = x ]; then					\
	  echo "Selecting OPAM based uninstall. ";			\
	  echo "Specify PREFIX=... for system-wide install.";		\
	  rm -fr `opam config var bin`/morbig 				\
                 `opam config var man`/morbig.1				\
	         `opam config var doc`/libmorbig			\
	  ocamlfind remove libmorbig;					\
	else								\
	  echo "Selecting system-wide install. PREFIX is $(PREFIX)";	\
	  rm -fr $(PREFIX)/bin/morbig					\
                 $(PREFIX)/share/man/man1/morbig.1			\
                 $(PREFIX)/share/doc/libmorbig;				\
	  ocamlfind remove -destdir $(PREFIX)/lib libmorbig;		\
         fi

tests:
	tests/run

clean:
	$(MAKE) -C src clean
	tests/run clean
	[ ! -d bin ] || rm -fr bin
	[ ! -d lib ] || rm -fr lib
