#!/bin/sh
set -e

fold_start () { printf 'travis_fold:start:%s\r\033[33;1m%s\033[0m\n' "$1" "$2"; }
fold_end () { printf 'travis_fold:end:%s\r' "$1"; }

## We need to install OPAM first. This cannot be done in the
## 'install:' directive of the .yml file, because we need a recent
## enough version that cannot be found in the packages. Besides, and
## contrary to OCaml, it does not take long to get installed.
fold_start install_opam 'Install OPAM...'
mkdir -p ~/.local/bin
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - \
    | sh -s ~/.local/bin system
export PATH=~/.local/bin:$PATH
fold_end   install_opam

fold_start prepare_opam 'Prepare OPAM...'
if [ -z "$OCAML_VERSION" ]; then
    printf 'No OCaml version provided in $OCAML_VERSION, using system.\n'
    OCAML_VERSION=system
fi
opam switch "$OCAML_VERSION"
eval $(opam config env)
opam update
opam upgrade -y
fold_end   prepare_opam

fold_start install_dependencies 'Install required dependencies...'
opam install -y menhir yojson ppx_deriving_yojson visitors
fold_end   install_dependencies

fold_start make 'Run `make`...'
make
fold_end   make

if [ -n "$RUN_TESTS" ]; then
    fold_start make_check 'Run the tests with `make check`...'
    make check
    fold_end   make_check
fi
