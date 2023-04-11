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

(** Raise in case of parsing error. *)
exception DuringParsing of Lexing.position

(** Raise in case of parsing error. *)
exception DuringLexing of Lexing.position * string

(** Raise in case of a limitation in alias handling. *)
exception DuringAliasing of Lexing.position * string

(** Raise in case of I/O error. *)
exception DuringIO of string

(** Returns a human-readable representation of Morbig's errors. *)
val string_of_error : exn -> string
