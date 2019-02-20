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
let is_name s =
  s <> ""
  && Str.(string_match (
              regexp "^\\([a-zA-Z]\\|_\\)\\([a-zA-Z]\\|_\\|[0-9]\\)*$") s 0)
