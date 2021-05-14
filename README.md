# Morbig

## A trustworthy static parser for POSIX shell

Morbig is a parser for shell scripts written in the POSIX shell script
language. It parses the scripts statically, that is without executing
them, and constructs a concrete syntax tree for each of them.  The
concrete syntax trees are built using constructors according to the
shell grammar of the POSIX standard.

## Download ![Build Status](https://github.com/colis-anr/morbig/actions/workflows/ci.yml/badge.svg?branch=master)

    git clone git@github.com:colis-anr/morbig.git

## License and Copyright

Please see the file COPYING.

## Documentation

You can have a look at the
[online API documentation](https://colis-anr.github.io/morbig/) or build
it yourself:

    make doc

## Are you in a hurry?

Yes? Pull our docker image:

    docker pull colisanr/morbig:latest

Then, define the following shell function:

    morbig () {
      D=$(cd "$(dirname "$1")"; pwd)
      B=$(basename "$1")
      docker run \
        -v "$D":/mnt \
        colisanr/morbig:latest --as simple /mnt/"$B"
    }

After that, you should be able to run ``morbig`` like this:

    morbig my-script.sh

This will create a JSON file named ``my-script.sh.sjson``.

You can also build a local docker image from the root of this
repository:

    docker build -t morbig . # to build a docker image with morbig inside.

Now if you want to use more features of ``morbig``, take the time to
follow the building instructions of the next section.

## Manual instructions

### Install using OPAM

Please type
``
opam install morbig
``
to get the latest public release of `morbig`.

If you want to use the development version of `morbig`, read the next sections.

### Dependencies

``morbig`` depends on the following software:

- dune
- menhir
- ocaml (≥ 4.04)
- odoc (for documentation only)
- yojson and ppx_deriving_yojson
- visitors

### Building

    make

### Installing

    make install            # for opam-based environments
    PREFIX=... make install # for system-wide install

### Testing

    make check
