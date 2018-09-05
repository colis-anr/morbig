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

(** This interface defines the API of libmorbig *)

(** {1 Parsing shell scripts} *)

(** [parse_file filename] performs the syntactic analysis of
   [filename] and returns a concrete syntax tree if [filename] content
   is syntactically correct.
   Raise {Errors.ParseError pos} or {Errors.LexicalError pos} otherwise. *)
val parse_file: string -> CST.program

(** [parse_string filename content] is similar to [parse_file] except the
    script source code is provided as a string. *)
val parse_string: string -> string -> CST.program

(** {1 Errors} *)
module Errors : sig

  (** Raise in case of parsing error. *)
  exception DuringParsing of Lexing.position

  (** Raise in case of parsing error. *)
  exception DuringLexing of Lexing.position * string

  (** Raise in case of I/O error. *)
  exception DuringIO of string

  (** Returns a human-readable representation of Morbig's errors. *)
  val string_of_error : exn -> string

end

(** {1 Serialization of CST} *)

(** [load_binary_cst cin] retrieves a serialized CST from
    input_channel [cin]. *)
val load_binary_cst: in_channel -> CST.program

(** [save_binary_cst cout cst] stores a serialized [cst] in [cout]. *)
val save_binary_cst: out_channel -> CST.program -> unit

(** [load_json_cst cin] retrieves a CST in JSON format from
   input_channel [cin]. *)
val load_json_cst: in_channel -> CST.program

(** [save_json_cst cout cst] stores a [cst] using JSON format in [cout]. *)
val save_json_cst: out_channel -> CST.program -> unit

(** [save_dot_cst cout cst] stores a [cst] using DOT format in [cout]. *)
val save_dot_cst: out_channel -> CST.program -> unit
