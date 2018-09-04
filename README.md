# Morbig
## A trustworthy static parser for POSIX shell

## Download [![Build Status](https://travis-ci.org/colis-anr/morbig.svg?branch=master)](https://travis-ci.org/colis-anr/morbig)


```
    git clone git@github.com:colis-anr/morbig.git
```

## License and Copyright

   please see the file COPYING

## Are you in a hurry?

   Yes? Pull our docker image:
```
   docker pull colisanr/morbig:latest
```

   Then, define the following shell function:

```
   morbig () {
      D=$(cd "$(dirname "$1")"; pwd)
      B=$(basename "$1")
      docker run \
         -v "$D":/mnt \
         colisanr/morbig:latest --as simple /mnt/"$B"
   }
```

   After that, you should be able to run ``morbig`` like this:

```
   morbig my-script.sh
```

   This will create a JSON file named ``my-script.sh.sjson``.

   You can also build a local docker image from the root of this repository:

```
   docker build -t morbig . # to build a docker image with morbig inside.
```

   Now if you want to use more features of ``morbig``, take the time
   to follow the building instructions of the next section.

## Building instructions

### Dependencies

``morbig`` depends on the following software:

```
    - ocaml               (>= 4.02.1)
    - ocamlbuild	  [ if ocaml >= 4.05 ]
    - menhir              (>= 20170509)
    - yojson              (>= 1.3.2)
    - ppx_deriving_yojson (>= 3.0)
    - visitors            (>= 20170404)
```

### Building

    make

### Installing

    make install            # for opam-based environments
    PREFIX=... make install # for system-wide install

### Testing

    make check
