#!/bin/sh
set -e

fold_start () { printf 'travis_fold:start:%s\r\033[33;1m%s\033[0m\n' "$1" "$2"; }
fold_end () { printf 'travis_fold:end:%s\r' "$1"; }

## Install OPAM
fold_start install_opam 'Install OPAM...'
mkdir -p ~/.local/bin
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - \
    | sh -s ~/.local/bin "$OCAML_VERSION"
export PATH=~/.local/bin:$PATH
eval $(opam config env)
fold_end install_opam

## OPAM dependencies
fold_start opam_deps 'Install OPAM dependencies...'
opam install -y menhir yojson ppx_deriving_yojson visitors
fold_end opam_deps
