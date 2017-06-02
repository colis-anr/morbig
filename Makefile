.PHONY: all debug clean
MAIN=morbigparser

all:
	$(MAKE) -C src
	mkdir -p bin
	cp src/$(MAIN).native bin/$(MAIN)

debug:
	DEBUGPARSING=yes $(MAKE) -C src
	mkdir -p bin
	cp src/(MAIN).native bin/(MAIN)

install:
	@ if [ x$(PREFIX) = x ]; then			\
	  echo ;					\
	  echo Please use the following command:;	\
	  echo;						\
	  echo % PREFIX=... make install;		\
	  echo ;					\
          echo 'to install (MAIN) at $$PREFIX/bin';	\
	  echo ;					\
	  exit 1;					\
	fi
	cp bin/(MAIN) $(PREFIX)/bin

clean:
	$(MAKE) -C src clean
	rm -f bin/shparser
	[ ! -d bin ] || rmdir bin
