.PHONY: build doc install uninstall check examples clean
.PHONY: dist docker-image publish-docker-image headers

EXPORTED_SOURCES=				\
	src/CST.ml				\
	src/errors.mli                          \
	src/API.mli

ODIR=$(shell ocamlc -where)

build:
	dune build @install
	[ -e bin ] || ln -sf _build/install/default/bin bin
	[ -e lib ] || ln -sf _build/install/default/lib/morbig lib

doc: build
	dune build @doc
	[ -e doc ] || ln -sf _build/default/_doc/_html doc

ifneq ($(PREFIX),)
INSTALL_ARGS := $(INSTALL_ARGS) --prefix $(PREFIX)
endif

ifneq ($(LIBDIR),)
INSTALL_ARGS := $(INSTALL_ARGS) --libdir $(LIBDIR)
endif

install:
	dune install $(INSTALL_ARGS)

uninstall:
	dune uninstall $(INSTALL_ARGS)

check:
	@ output=$$(./tests/run 2>&1) ;     \
	  status=$$? ;                      \
	  echo "$$output" | tee tests.org ; \
	  exit $$status

examples:
	find examples -name 'Makefile' | \
	    while read file; do dirname "$$file"; done | \
	    xargs -n1 make -C

clean:
	dune clean
	rm -f bin lib doc
	rm -f tests.org
	tests/run clean

PACKAGE=$(shell echo morbig-`cat VERSION`)

dist: clean
	git archive -o $(PACKAGE).tar --format tar --prefix $(PACKAGE)/  master
	gzip -9 $(PACKAGE).tar

docker-image: Dockerfile
	@docker build -t morbig .

publish-docker-image: docker-image
	docker tag morbig colisanr/morbig:latest
	docker image push colisanr/morbig:latest

headers:
	cd src && headache -c .headache.conf -h .header *.ml *.ml[ily]
