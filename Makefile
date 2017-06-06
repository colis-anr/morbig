.PHONY: all debug clean

all:
	$(MAKE) -C src
	mkdir -p bin lib
	cp src/morbig.native bin/morbig
	cp src/_build/libmorbig.* lib
	# cp src/_build/dllmorbig.so lib

debug:
	DEBUGPARSING=yes $(MAKE) -C src
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
	cp lib/* src/_build/CST.cmi src/_build/CST.ml src/_build/API.cmi src/_build/API.mli \
            `ocamlfind printconf destdir`/libmorbig
	ocamlfind install libmorbig META || true

clean:
	$(MAKE) -C src clean
	[ ! -d bin ] || rm -fr bin
	[ ! -d lib ] || rm -fr lib
