#!/bin/sh
set -euC

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

## The following function recognizes anything of the form:
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
## where $1 is the first argument. It is useful to copy a whole type while
## keeping it equal with the original module's type. In turn, this allows to
## define derivers in an other module that will work on the types from the
## initial module. In a way, this is a cheap version of ppx_import.

add_type_equalities () {
    sed "s/\(type\|and\)\(.* \)\([a-zA-Z_'][a-zA-Z_']*\) *=\([^=]*=\)\? *\([^ a-z]\|$\)/\1\2\3 = \2CST.\3 = \5/g"
}

cat CST.ml | \
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
