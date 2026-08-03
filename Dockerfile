## Get an OPAM image. The tag can be changed by the `--build-arg`
## argument to the `docker build` command line. The full image name
## can also be changed by the same mean.

ARG tag=alpine
ARG image=ocaml/opam:$tag
FROM $image
MAINTAINER Yann Regis-Gianas

## Choose the right version of the OPAM switch. By default, we use the
## one coming from the dist image. But one can specify a specific
## switch with the `--build-arg`.

ARG switch=
RUN if [ -n "$switch" ]; then opam switch create "$switch"; fi

## Install dependencies. `opam depext` installs (in a distribution independant
## way) first the non-opam dependencies that are required and then the OPAM
## packages.

RUN opam depext -i menhir yojson ppx_deriving_yojson visitors

## Install documentation dependencies. Disabled by default, but can be enabled
## with `--build-arg doc=true`.

ARG doc=false
RUN if $doc; then opam depext -i odoc; fi

## Install tests dependencies. Disabled by default, but be enabled with
## `--build-arg tests=true`.

ARG tests=false
RUN if $tests; then opam depext -i conf-jq alcotest qcheck qcheck-alcotest; fi

## Work in /home/opam/morbig, copy all the file there with the right
## owner and group.

WORKDIR /home/opam/morbig
ADD . .
RUN sudo chown -R opam .

## Build Morbig

RUN opam exec -- make

## Set up the entry point of this Dockerfile to Morbig's binary that
## has just been built.

ENTRYPOINT [ "/home/opam/morbig/bin/morbig" ]
