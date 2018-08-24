.PHONY: all debug clean checks uninstall doc install install-local

EXPORTED_SOURCES=				\
	src/CST.ml				\
	src/API.mli

all:
	$(MAKE) -C src
	mkdir -p bin lib
	if [ -e src/morbig.native ]; then \
		cp src/morbig.native bin/morbig ;\
		cp src/_build/libmorbig.o src/_build/libmorbig.cm* \
			src/_build/libmorbig.a src/_build/libmorbigc.a lib; \
	else \
		cp src/morbig.byte bin/morbig ;\
		cp src/_build/libmorbig.cm* src/_build/libmorbigc.a lib; \
	fi

doc:
	$(MAKE) -C src doc
	cp -fr src/libmorbig.docdir/* doc

debug:
	DEBUGPARSING=yes $(MAKE) -C src debug
	mkdir -p bin
	cp src/morbig.byte bin/morbig

install:
	@ if [ x$(PREFIX) = x ]; then						\
	  echo "Selecting OPAM based install.";					\
	  echo "Specify PREFIX=... for system-wide install.";			\
	  cp bin/morbig `opam config var bin`;					\
	  cp man/morbig.1 `opam config var man`;				\
	  mkdir -p `opam config var doc`/libmorbig;				\
	  cp -fr doc/* `opam config var doc`/libmorbig;				\
	  ocamlfind remove libmorbig;						\
	  ocamlfind install libmorbig META lib/* include/* $(EXPORTED_SOURCES);	\
	else									\
	  echo "Selecting system-wide install. PREFIX is $(PREFIX)";		\
	  mkdir -p $(PREFIX)/bin;						\
	  cp bin/morbig $(PREFIX)/bin;						\
	  mkdir -p $(PREFIX)/share/man/man1;					\
	  cp man/morbig.1 $(PREFIX)/share/man/man1;				\
	  mkdir -p $(PREFIX)/share/doc/libmorbig;				\
	  cp -fr doc/* $(PREFIX)/share/doc/libmorbig;				\
	  mkdir -p $(PREFIX)/lib/ocaml;						\
	  ocamlfind remove -destdir $(PREFIX)/lib/ocaml libmorbig;		\
	  ocamlfind install -destdir $(PREFIX)/lib/ocaml			\
	  libmorbig META $(EXPORTED_SOURCES) lib/* include/*;			\
         fi

install-local:
	PREFIX=/usr/local make install

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

check:
	@ output=$$(./tests/run 2>&1) ;     \
	  status=$$? ;                      \
	  echo "$$output" | tee tests.org ; \
	  exit $$status

VERSION := $(shell cat src/VERSION)
NAME_VERSION := morbig.${VERSION}
ARCHIVE := https://github.com/colis-anr/morbig/archive/v${VERSION}.tar.gz
CHECKSUM := $$(wget -qO- "${ARCHIVE}" | md5sum | cut -d ' ' -f 1)

opam-release:
	mkdir "${NAME_VERSION}"
	cp descr opam "${NAME_VERSION}"
	printf 'archive: "%s"\nchecksum: "%s"\n' "${ARCHIVE}" "${CHECKSUM}" > "${NAME_VERSION}"/url
	@printf 'Check the content of ${NAME_VERSION}. When happy, run:\nopam publish submit ${NAME_VERSION}\n'

clean:
	$(MAKE) -C src clean
	rm -f src/version.ml
	rm -f tests.org
	rm -rf "${NAME_VERSION}"
	tests/run clean
	[ ! -d bin ] || rm -fr bin
	[ ! -d lib ] || rm -fr lib
