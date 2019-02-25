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

let parse_file = Scripts.parse_file

let parse_string = Scripts.parse_string

let load_binary_cst cin =
  (input_value cin : CST.program)

let save_binary_cst cout cst =
  output_value cout cst

let load_json_cst cin =
  JsonHelpers.load_from_json cin

let save_json_cst cout cst =
  JsonHelpers.save_as_json false cout cst

let save_dot_cst cout cst =
  JsonHelpers.save_as_dot cout cst

let on_located = CSTHelpers.on_located

let start_of_position = CSTHelpers.start_of_position

let end_of_position = CSTHelpers.end_of_position

let filename_of_position = CSTHelpers.filename_of_position

let string_of_lexing_position = CSTHelpers.string_of_lexing_position

let remove_quotes = QuoteRemoval.on_string
