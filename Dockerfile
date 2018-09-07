## Get an OPAM image. By default, OCaml 4.06, but the tag can be
## changed by the `docker build` command line.

ARG tag=4.06
ARG image=ocaml/opam2:$tag
FROM $image
MAINTAINER Yann Regis-Gianas

## Install dependencies. `opam depext` installs first the non-opam
## dependencies that are required and then the OPAM packages.

RUN opam depext -i menhir yojson ppx_deriving_yojson visitors

## Work in /home/opam/morbig, copy all the file there with the right
## owner and group.

WORKDIR /home/opam/morbig
ADD . .
RUN sudo chown -R opam .

## Build Morbig

RUN eval $(opam env) && make

## Set up the entry point of this Dockerfile to Morbig's binary that
## has just been built.

ENTRYPOINT [ "/home/opam/morbig/bin/morbig" ]
