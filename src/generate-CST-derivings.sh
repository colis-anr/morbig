#!/bin/sh
############################################################################
##  Copyright (C) 2017-2021 Yann Régis-Gianas, Nicolas Jeannerod,         ##
##  Ralf Treinen.                                                         ##
##                                                                        ##
##  This is free software: you can redistribute it and/or modify it       ##
##  under the terms of the GNU General Public License, version 3.         ##
##                                                                        ##
##  Additional terms apply, due to the reproduction of portions of        ##
##  the POSIX standard. Please refer to the file COPYING for details.     ##
############################################################################

## The goal of this script is to copy the type definitions of the module CST and
## to add code to it. Typically, this code will be obtained using [@@deriving]
## annotations to produce serializers or visitors.
##
## In an ideal world, this could be done fully in OCaml using ppx_import. This
## PPX seems however not to be maintained so much anymore. It also clashes with
## the ocaml-migrate-parsetree way to write PPXes, which is used both in
## ppx_deriving and visitors, which we use. In lack of a better solution, Shell
## comes to the rescue!

set -euC

## The first argument of this script represents the kind of file which we want
## to produce. For now, this script only supports "serializers" and "visitors".
## For each of these kinds, we define two strings which will be put after the
## introduction or after the CST. Typically, these strings will contain
## [@@deriving] annotations in order to produce extra code from the types in the
## CST file.

readonly KIND=$1

case $KIND in
    serializers)
        INTRO_DERIVERS='[@@deriving yojson]'
        CST_DERIVERS='[@@deriving yojson]'
        ;;

    visitors)
        INTRO_DERIVERS='
[@@deriving
  visitors { variety = "iter";      name = "located_iter";      polymorphic = true },
  visitors { variety = "map";       name = "located_map";       polymorphic = true },
  visitors { variety = "reduce";    name = "located_reduce";    polymorphic = true },
  visitors { variety = "mapreduce"; name = "located_mapreduce"; polymorphic = true },
  visitors { variety = "iter2";     name = "located_iter2";     polymorphic = true },
  visitors { variety = "map2";      name = "located_map2";      polymorphic = true },
  visitors { variety = "reduce2";   name = "located_reduce2";   polymorphic = true }
]'
        CST_DERIVERS='
[@@deriving
  visitors { variety = "iter";       ancestors = ["located_iter"];      nude = true },
  visitors { variety = "map";        ancestors = ["located_map"];       nude = true },
  visitors { variety = "reduce";     ancestors = ["located_reduce"];    nude = true },
  visitors { variety = "mapreduce";  ancestors = ["located_mapreduce"]; nude = true },
  visitors { variety = "iter2";      ancestors = ["located_iter2"];     nude = true },
  visitors { variety = "map2";       ancestors = ["located_map2"];      nude = true },
  visitors { variety = "reduce2";    ancestors = ["located_reduce2"];   nude = true }
]'
        ;;

    *)
        printf 'I do not know what to do with kind %s.\n' "$KIND"
        exit 2
esac

## The following sed recognizes anything of the form:
##
##     type foo = ...
##     type foo = bar = ...
##     and foo = ...
##     and foo = bar = ...
##
## and replaces them with:
##
##     type foo = $1.foo = ...
##     type foo = $1.foo = ...
##     and foo = $1.foo = ...
##     and foo = $1.foo = ...
##
## where $1 is the first argument, except when what follows the equal sign
## starts with a lowercase, because this would then be a syntax equality and we
## must not add an other one. This function is useful to copy a whole type while
## keeping it equal with the original module's type.

add_type_equalities () {
    sed "s/\(type\|and\)\(.* \)\([a-zA-Z_'][a-zA-Z_']*\) *=\([^=]*=\)\? *\([^ a-z]\|$\)/\1\2\3 = \2CST.\3 = \5/g"
}

## Here, we copy the type CST, we rewrite all the type definitions to add a type
## equality with the CST and then we replace the placeholders by their contents.

cat CST.mli | \
    add_type_equalities CST | \
    while read -r line; do
        case $line in
            '(*** INTRO_DERIVERS ***)')
                echo "$INTRO_DERIVERS"
                ;;
            '(*** CST_DERIVERS ***)')
                echo "$CST_DERIVERS"
                ;;
            *)
                echo "$line"
        esac
    done
