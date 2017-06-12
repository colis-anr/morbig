.PHONY: all debug clean tests

all:
	$(MAKE) -C src
	mkdir -p bin lib
	cp src/morbig.native bin/morbig
	cp src/_build/libmorbig.* lib
	# cp src/_build/dllmorbig.so lib

debug:
	DEBUGPARSING=yes $(MAKE) -C src debug
	mkdir -p bin
	cp src/morbig.native bin/morbig

install:
	@ if [ x$(PREFIX) = x ]; then			\
	  echo ;					\
	  echo Please use the following command:;	\
	  echo;						\
	  echo % PREFIX=... make install;		\
	  echo ;					\
          echo 'to install morbig at $$PREFIX/bin';	\
	  echo ;					\
	  exit 1;					\
	fi
	cp bin/morbig $(PREFIX)/bin
	mkdir -p $(PREFIX)/share/man/man1
	cp man/morbig.1 $(PREFIX)/share/man/man1
	ocamlfind install libmorbig META || true
	cp lib/* src/_build/CST.cmi src/_build/CST.ml src/_build/API.cmi src/_build/API.mli \
            `ocamlfind printconf destdir`/libmorbig

tests:
	tests/run

clean:
	$(MAKE) -C src clean
	tests/run clean
	[ ! -d bin ] || rm -fr bin
	[ ! -d lib ] || rm -fr lib
