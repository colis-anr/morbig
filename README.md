# Morbig

## A trustworthy static parser for POSIX shell

Morbig is a parser for shell scripts written in the POSIX shell script
language. It parses the scripts statically, that is without executing
them, and constructs a concrete syntax tree for each of them.  The
concrete syntax trees are built using constructors according to the
shell grammar of the POSIX standard.

## Download ![Build Status](https://github.com/colis-anr/morbig/actions/workflows/ci.yml/badge.svg)

    git clone git@github.com:colis-anr/morbig.git

## License and copyright

See [COPYING](./COPYING).

## Documentation

For tagged versions, you can have a look at the [official documentation]. We
also provide [custom documentation] for the latest development version. Finally,
you can always build the documentation yourself with:

    make doc

[official documentation]: https://ocaml.org/p/morbig/latest/doc/
[custom documentation]: https://colis-anr.github.io/morbig/

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

After that, you should be able to run Morbig like this:

    morbig my-script.sh

This will create a JSON file named `my-script.sh.sjson`.

You can also build a local docker image from the root of this
repository:

    docker build -t morbig .

Now if you want to use more features of Morbig, take the time to follow the
building instructions of the next section.

## Manual instructions

### Install using OPAM

Please type

    opam install morbig

to get the latest public release of Morbig.

If you want to use the development version of Morbig, read the next sections.

### Dependencies

Morbig depends on the following software:

- dune
- menhir
- OCaml ≥ 4.11. Older versions might work but are not officially supported.
- odoc (for documentation only)
- yojson and ppx_deriving_yojson
- visitors

In case of discrepancies on dependencies, [`dune-project`](./dune-project) is
the source of truth. Our continuous integration ensures that it is up-to-date.

### Building

    make

### Installing

For OPAM-based environments:

    make install

For system-wide installs:

    PREFIX=... make install

### Testing

    make check
