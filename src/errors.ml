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

exception DuringParsing of Lexing.position

exception DuringLexing of Lexing.position * string

exception DuringAliasing of Lexing.position * string

exception DuringIO of string

let string_of_error = function
  | DuringParsing pos ->
     Printf.sprintf "%s: Syntax error."
       CSTHelpers.(string_of_lexing_position pos)
  | DuringLexing (pos, msg) ->
     Printf.sprintf "%s: Lexical error (%s)."
       CSTHelpers.(string_of_lexing_position pos)
       msg
  | DuringIO msg ->
     Printf.sprintf "Input/Output error (%s)." msg
  | DuringAliasing (pos,msg) ->
     Printf.sprintf "%s: Alias handling limitation (%s)."
       CSTHelpers.(string_of_lexing_position pos)
       msg
  | Failure s ->
     "Failure: " ^ s ^ "."
  | Sys_error s ->
     "Error: " ^ s ^ "."
  | e -> raise e
