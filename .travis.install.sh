#!/bin/sh
set -e

fold_start () { printf 'travis_fold:start:%s\r\033[33;1m%s\033[0m\n' "$1" "$2"; }
fold_end () { printf 'travis_fold:end:%s\r' "$1"; }

## APT dependencies
fold_start apt_deps 'Install APT dependencies...'
sudo apt-get install ocaml aspcud
fold_end apt_deps

## Install OPAM
fold_start install_opam 'Install OPAM...'
mkdir -p ~/.local/bin
wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - \
    | sh -s ~/.local/bin system
export PATH=~/.local/bin:$PATH
fold_end install_opam

## Prepare OPAM
fold_start prep_opam 'Prepare OPAM...'
if [ -z "$OCAML_VERSION" ]; then
    printf 'No OCaml version provided in $OCAML_VERSION, using $DEFAULT_OCAML_VERSION (%s).\n' "$DEFAULT_OCAML_VERSION"
    OCAML_VERSION=$DEFAULT_OCAML_VERSION
fi
opam switch "$OCAML_VERSION"
eval $(opam config env)
fold_end prep_opam

## OPAM dependencies
fold_start opam_deps 'Install OPAM dependencies...'
opam install -y menhir yojson ppx_deriving_yojson visitors
fold_end opam_deps
