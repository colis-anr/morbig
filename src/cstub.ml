(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Nicolas Jeannerod,    *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

external _dummy_external : unit -> unit = "dummy_external"
(* This dummy external is here to add a virtual dependency between
   this module and the C stubs. If we don't have such a dependency,
   OCaml (or ld) optimizes the code and removes the C stubs. *)

let _ =
  Callback.register "untyped_parse_file" CAPI.untyped_parse_file;
  Callback.register "dispose_cst" CAPI.dispose_cst
