(**************************************************************************)
(*  Copyright (C) 2017-2023 Yann Régis-Gianas, Nicolas Jeannerod,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

(* This module is not part of Morbig itself. Its only purpose is to help build
   Morbig by copying the type definitions of the module CST and adding code to
   it. Typically, this code will be obtained using [@@deriving] annotations to
   produce serializers or visitors.

   In an ideal world, this could be done directly using ppx_import. However:

   - ppx_import seems not to be maintained so much anymore. It also clashes with
     the ocaml-migrate-parsetree way to write PPXes, which is used both in
     ppx_deriving and visitors, which we use.

   - We are unsure whether ppx_import could tell the documentation not to build
     some modules, as odoc only works textually.

   In lack of a better solution, string manipulations comes to the rescue! *)

type placeholders_content =
  { prelude : string ;
    intro_derivers : string ;
    cst_derivers : string }

let placeholders_content =
  match Sys.argv.(1) with

  | "serializers" ->
    { prelude = "" ;
      intro_derivers = "[@@deriving yojson]" ;
      cst_derivers = "[@@deriving yojson]" }

  | "visitors" ->
    {
      prelude = {|
(** Visitors for concrete syntax trees.

   This module contains all the visitors that can be generated using
   {{: https://gitlab.inria.fr/fpottier/visitors }  the PPX visitors library },
   namely: {ul {li iter} {li map} {li reduce} {li mapreduce} {li iter2}
   {li map2} {li reduce2}}

   The generated type for visitors on the whole concrete syntax tree can be
   huge, which makes them unreadable and the files containing them ridiculously
   big. They are therefore not generated and not shown here. We refer the user
   to {{: http://gallium.inria.fr/~fpottier/visitors/manual.pdf } the
   documentation of visitors }. *)

(* The following disables odoc for the rest of the file. *)
(**/**)
 |} ;

      intro_derivers = {|
[@@deriving
  visitors { variety = "iter";      name = "located_iter";      polymorphic = true },
  visitors { variety = "map";       name = "located_map";       polymorphic = true },
  visitors { variety = "reduce";    name = "located_reduce";    polymorphic = true },
  visitors { variety = "mapreduce"; name = "located_mapreduce"; polymorphic = true },
  visitors { variety = "iter2";     name = "located_iter2";     polymorphic = true },
  visitors { variety = "map2";      name = "located_map2";      polymorphic = true },
  visitors { variety = "reduce2";   name = "located_reduce2";   polymorphic = true }
]
        |} ;

      cst_derivers = {|
[@@deriving
  visitors { variety = "iter";       ancestors = ["located_iter"];      nude = true },
  visitors { variety = "map";        ancestors = ["located_map"];       nude = true },
  visitors { variety = "reduce";     ancestors = ["located_reduce"];    nude = true },
  visitors { variety = "mapreduce";  ancestors = ["located_mapreduce"]; nude = true },
  visitors { variety = "iter2";      ancestors = ["located_iter2"];     nude = true },
  visitors { variety = "map2";       ancestors = ["located_map2"];      nude = true },
  visitors { variety = "reduce2";    ancestors = ["located_reduce2"];   nude = true }
]
        |}
    }

  | arg ->
    Format.eprintf "I do not know what do with my first argument, `%s`.@." arg;
    exit 2

  | exception Invalid_argument _ ->
    Format.eprintf "I expect a first argument.@.";
    exit 2

(* Get the content of the CST.mli file. *)

let file_content =
  let fname = "CST.mli" in
  let ichan = open_in fname in
  let buflen = 1024 in
  let out = Buffer.create buflen in
  let buf = Bytes.create buflen in
  let rec read () =
    match input ichan buf 0 buflen with
    | 0 -> ()
    | n -> Buffer.add_subbytes out buf 0 n; read ()
  in
  read ();
  close_in ichan;
  Buffer.contents out

(* Add type equalities to all type definitions. *)

let type_foo_eq_regexp =           (* this regular expression matches: *)
  "\\(type\\|and\\)\\([^=\n]* \\)" (* "type" or "and" followed by anything that ends by a space *)
  ^ "\\([^ =\n]+\\) *="            (* anything that looks like a type identifier, followed by = *)
  ^ "\\([^=\n]*=\\|\\) *"          (* possibly some other stuff followed by equal (type equality) *)
  ^ "\\([^ a-z\n][^=\n]*\\|\\)"    (* anything that does not start with a lowercase character *)
  ^ "$"                            (* because that would also be a type equality *)
let type_foo_eq_regexp = Str.regexp type_foo_eq_regexp
(* Note: there can be type equalities that do not syntactically start with a
   lowercase character (eg. when there is a module name involved), but the
   current regexp is good enough for our use and our definition of the CST. *)

let type_foo_eq_repl = {|\1\2\3 = \2CST.\3 = \5|}
(* The replacement is of the form "type foo = CST.foo = ..." *)

let file_content = Str.global_replace type_foo_eq_regexp type_foo_eq_repl file_content

(* Replace the placeholders. *)

let replace_placeholder placeholder replacement file_content =
  let regexp = Str.regexp_string placeholder in
  Str.global_substitute regexp (fun _ -> replacement) file_content

let file_content = replace_placeholder "(*** PRELUDE ***)" placeholders_content.prelude file_content
let file_content = replace_placeholder "(*** INTRO_DERIVERS ***)" placeholders_content.intro_derivers file_content
let file_content = replace_placeholder "(*** CST_DERIVERS ***)" placeholders_content.cst_derivers file_content

(* Print the result. *)

let () = print_string file_content
