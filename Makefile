.PHONY: all debug clean checks uninstall doc install install-local dist examples
.PHONY: docker-image publish-docker-image

EXPORTED_SOURCES=				\
	src/CST.ml				\
	src/errors.mli                          \
	src/API.mli

all:
	$(MAKE) -C src
	mkdir -p bin lib
	if [ -e src/morbigDriver.native ]; then \
		cp src/morbigDriver.native bin/morbig ;\
		cp src/_build/morbig.o src/_build/morbig.cm* \
			src/_build/morbig.a src/_build/libmorbigc.a lib; \
	else \
		cp src/morbigDriver.byte bin/morbig ;\
		cp src/_build/morbig.cm* src/_build/libmorbigc.a lib; \
	fi

doc:
	$(MAKE) -C src doc
	cp -fr src/morbig.docdir/* doc

debug:
	DEBUGPARSING=yes $(MAKE) -C src debug
	mkdir -p bin
	cp src/morbigDriver.byte bin/morbig

install:
	@ if [ x$(PREFIX) = x ]; then						\
	  echo "Selecting OPAM based install.";					\
	  echo "Specify PREFIX=... for system-wide install.";			\
	  cp bin/morbig `opam config var bin`;					\
	  cp man/morbig.1 `opam config var man`;				\
	  mkdir -p `opam config var doc`/morbig;				\
	  cp -fr doc/* `opam config var doc`/morbig;				\
	  ocamlfind remove morbig;						\
	  ocamlfind install morbig META lib/* include/* $(EXPORTED_SOURCES);	\
	else									\
	  echo "Selecting system-wide install. PREFIX is $(PREFIX)";		\
	  mkdir -p $(PREFIX)/bin;						\
	  cp bin/morbig $(PREFIX)/bin;						\
	  mkdir -p $(PREFIX)/share/man/man1;					\
	  cp man/morbig.1 $(PREFIX)/share/man/man1;				\
	  mkdir -p $(PREFIX)/share/doc/morbig; 					\
	  cp -fr doc/* $(PREFIX)/share/doc/morbig;				\
	  mkdir -p $(PREFIX)/lib/ocaml;						\
	  ocamlfind remove -destdir $(PREFIX)/lib/ocaml morbig;			\
	  ocamlfind install -destdir $(PREFIX)/lib/ocaml			\
	    morbig META $(EXPORTED_SOURCES) lib/* include/*;			\
         fi

install-local:
	PREFIX=/usr/local make install

uninstall:
	@ if [ x$(PREFIX) = x ]; then					\
	  echo "Selecting OPAM based uninstall. ";			\
	  echo "Specify PREFIX=... for system-wide install.";		\
	  rm -fr `opam config var bin`/morbig 				\
                 `opam config var man`/morbig.1				\
	         `opam config var doc`/morbig				\
	  ocamlfind remove morbig;					\
	else								\
	  echo "Selecting system-wide install. PREFIX is $(PREFIX)";	\
	  rm -fr $(PREFIX)/bin/morbig					\
                 $(PREFIX)/share/man/man1/morbig.1			\
                 $(PREFIX)/share/doc/morbig;				\
	  ocamlfind remove -destdir $(PREFIX)/lib morbig;		\
         fi

check:
	@ output=$$(./tests/run 2>&1) ;     \
	  status=$$? ;                      \
	  echo "$$output" | tee tests.org ; \
	  exit $$status

examples:
	find examples -name 'Makefile' | \
	    while read file; do dirname "$$file"; done | \
	    xargs -n1 make -C

opam-release:
	opam publish --split

clean:
	$(MAKE) -C src clean
	rm -f src/version.ml
	rm -f tests.org
	tests/run clean
	[ ! -d bin ] || rm -fr bin
	[ ! -d lib ] || rm -fr lib

PACKAGE=$(shell echo morbig-`cat VERSION`)

dist:
	git archive -o $(PACKAGE).tar --format tar --prefix $(PACKAGE)/  master
	gzip -9 $(PACKAGE).tar

docker-image: Dockerfile
	@docker build -t morbig .

publish-docker-image: docker-image
	docker tag morbig colisanr/morbig:latest
	docker image push colisanr/morbig:latest
