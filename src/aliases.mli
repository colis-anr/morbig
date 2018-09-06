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

(* handling the alias and unalias builtins when they occur at toplevel *)

(** Type for a table of active aliases. *)
type t

(** [empty] is an empty alias table. *)
val empty : t

(** [interpret aliases cst] traverses [cst] to check that there are no
    alias or unalias invocations in a nested command (that is, a
    compound command, or a function definition). These are allowed in
    POSIX, but we have to refuse them as they cannot be expanded
    statically.
    - if this is the case then it returns an alias table which is obtained
      from [aliases] by executing all alias and unalias invocations in [cst];
    - if this is not the case then it raises [Errors.DuringAliasing]. *)
val interpret :
  t -> CST.complete_command -> t

(** [alias_substitution aliases checkpoint word] substitutes an
    alias by its definition if [word] is not a reserved word and
    if the parsing context is about to reduce a [cmd_name]. *)
val alias_substitution :
  t -> 'a Parser.MenhirInterpreter.checkpoint -> string -> t * string
