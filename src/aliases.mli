(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018 Yann Régis-Gianas, Nicolas Jeannerod,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

(* handling the alias and unalias buildins when they occur at toplevel *)

(** Type for a table of active aliases. *)
type t = (string * string) list
(** [empty] is an empty alias table. *)
val empty : t

(** Exception raised in case of an alias or unalias invocation inside
    a composite command. These are allowed in POSIX, but we have to
    refuse them as they cannnot be expanded statically. *)
exception NestedAliasingCommand

(** [interpret aliases cst] traverses [cst] to check that there are no
    alias or unalias invocations in a nested command:
    - if this is the case then it returns an alias table which is obtained
      from [aliases] by executing all alias and unalias invocations in [cst]; 
    - if this is not the case then it raises [NestedAliasingCommand]. *)
val interpret :
  t -> CST.complete_command -> t

(** [alias_substitution aliases checkpoint word] substitutes an
    alias by its definition if [word] is not a reserved word and
    if the parsing context is about to reduce a [cmd_name]. *)
val alias_substitution :
  t -> 'a Parser.MenhirInterpreter.checkpoint -> string -> string
