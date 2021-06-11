(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017-2021 Yann Régis-Gianas, Nicolas Jeannerod,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

(** Serializers for concrete syntax trees. *)

val program_to_yojson : CST.program -> Yojson.Safe.t

val program_of_yojson : Yojson.Safe.t -> CST.program Ppx_deriving_yojson_runtime.error_or

val bracket_expression_to_yojson : CST.bracket_expression -> Yojson.Safe.t
val bracket_expression_of_yojson : Yojson.Safe.t -> CST.bracket_expression Ppx_deriving_yojson_runtime.error_or
