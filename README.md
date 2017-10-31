# Morbig
## A trustworthy parser for POSIX shell

## Download

    git clone https://gitlab.inria.fr/regisgia/morbig.git

## License and Copyright

   please see the file COPYING

## Building instructions

### Dependencies

    morbig depends on the following software:
    - ocaml
    - menhir              (>= 20170509)
    - yojson              (>= 1.3.3)
    - ppx_deriving_yojson (>= 3.0)
    - visitors	          (>=

### Building

    make

### Installing

    make install            # for opam-based environments
    PREFIX=... make install # for system-wide install

### Testing

    make tests
