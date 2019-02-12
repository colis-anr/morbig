.PHONY: build doc install uninstall check examples clean
.PHONY: dist docker-image publish-docker-image

EXPORTED_SOURCES=				\
	src/CST.ml				\
	src/errors.mli                          \
	src/API.mli

ODIR=$(shell ocamlc -where)

build:
	dune build @install

doc: build
	dune build @doc
	mkdir -p doc && cp -rf _build/ doc

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
	rm -f src/version.ml
	rm -f tests.org
	tests/run clean
	[ ! -d bin ] || rm -rf bin
	[ ! -d lib ] || rm -rf lib

PACKAGE=$(shell echo morbig-`cat VERSION`)

dist: clean
	git archive -o $(PACKAGE).tar --format tar --prefix $(PACKAGE)/  master
	gzip -9 $(PACKAGE).tar

docker-image: Dockerfile
	@docker build -t morbig .

publish-docker-image: docker-image
	docker tag morbig colisanr/morbig:latest
	docker image push colisanr/morbig:latest
