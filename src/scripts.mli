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

(** [is_other_script s] returns [true] when the file with name [s]
    starts with a magic string that indicates a script other than
    posix shell, otherwise it returns [false]. Raises [Sys_error]
    when the file cannot be opened. *)
val is_other_script: string -> bool

(** [is_elf s] returns [true] when the file with name [s] starts with
    the magic number for ELF, otherwise it returns [false].
    Raises [Sys_error] when the file cannot be opened. *)
val is_elf: string -> bool

(** [parse_file s] attempts to parse the file with name [s], and returns
    its concrete syntax tree. *)
val parse_file: string -> CST.program

(** [parse_string s c] attempts to parse the file with name [s] whose contents
    is [c], and returns its concrete syntax tree. *)
val parse_string: string -> string -> CST.program
