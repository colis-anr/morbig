.PHONY: all debug clean

all:
	$(MAKE) -C src
	mkdir -p bin lib
	cp src/morbig.native bin/morbig
	cp _build/src/libmorbig.* _build/src/dllmorbig.so lib

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

clean:
	$(MAKE) -C src clean
	[ ! -d bin ] || rm -fr bin
	[ ! -d lib ] || rm -fr lib
