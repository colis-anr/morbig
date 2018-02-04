FROM ocaml/opam
MAINTAINER Yann Regis-Gianas

RUN opam update
RUN opam switch 4.02.3
RUN eval `opam config env`
RUN opam install menhir yojson ppx_deriving_yojson visitors
RUN echo 'Compiling Morbig development version.'
RUN git clone https://github.com/colis-anr/morbig.git
RUN eval `opam config env` && cd morbig && make
ENTRYPOINT [ "/home/opam/morbig/bin/morbig" ]