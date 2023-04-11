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

(*specification:

   3.231 Name

   In the shell command language, a word consisting solely of
   underscores, digits, and alphabetics from the portable character
   set. The first character of a name is not a digit.

   Note:
   The Portable Character Set is defined in detail in Portable Character Set.

*)
(**
    This definition implies that a name is not empty.

*)
let alpha c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || (c = '_')

let alphanum c =
  alpha c || ('0' <= c && c <= '9')

let is_name s =
  let len = String.length s in
  let rec aux i =
    i = len || (alphanum s.[i] && aux (i + 1))
  in
  if len = 0 then false
  else if not (alpha s.[0]) then false
  else aux 0
