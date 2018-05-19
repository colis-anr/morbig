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

type t =
  | Backquotes of char * int
  | Parentheses
  | Braces
  | DQuotes

let to_string = function
  | Backquotes (c, level) -> Printf.sprintf "@%c[%d]" c level
  | Parentheses -> "("
  | Braces -> "{"
  | DQuotes -> "\""

let of_opening c =
  if c = '(' then Parentheses
  else if c = '{' then Braces
  else if c = '`' then Backquotes (c, 0) (* FIXME *)
  else failwith "Unrecognized nesting."

let of_closing c =
  if c = ')' then Parentheses
  else if c = '}' then Braces
  else if c = '`' then Backquotes (c, 0) (* FIXME *)
  else failwith "Unrecognized nesting."
