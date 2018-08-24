#!/bin/sh
set -e

fold_start () { printf 'travis_fold:start:%s\r\033[33;1m%s\033[0m\n' "$1" "$2"; }
fold_end () { printf 'travis_fold:end:%s\r' "$1"; }

## Initialize OPAM
export PATH=~/.local/bin:$PATH
eval "$(opam config env)"
opam update
opam upgrade -y

## Make
fold_start make 'Build the project'
make
fold_end make

## Make install
if [ -n "$INSTALL" ]; then
    fold_start install 'Install the project'
    make install
    fold_end install

    fold_start uninstall 'Uninstall the project'
    make uninstall
    fold_end uninstall
fi

## Run tests
if [ -n "$RUN_TESTS" ]; then
    fold_start tests 'Run the tests'
    make check
    fold_end tests
fi
