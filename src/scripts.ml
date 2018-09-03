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

let other_scripts_magic_strings =
  List.map Str.regexp [
             "#![ ]*/usr/bin/perl.*";
             "#![ ]*/bin/bash.*"
           ]

let is_other_script filename =
  (* check whether [filename] is a script other than /bin/sh *)
  let cin = open_in filename in
  try
    let firstline = input_line cin in
    close_in cin;
    List.exists
      (function r -> Str.string_match r firstline 0)
      other_scripts_magic_strings
  with End_of_file ->
     (** An empty file is not considered as a script.*)
     false

let elf_magic_number = Bytes.of_string  "\x7FELF"

let is_elf filename =
  (* check whether [filename] is an ELF executable *)
  let cin = open_in_bin filename
  and buf = Bytes.create 4 in
  let number_chars_read = input cin buf 0 4 in
  begin
    close_in cin;
    if number_chars_read < 4
    then false
    else Bytes.compare buf elf_magic_number = 0
  end

let parse_string filename contents =
  let lexbuf = ExtPervasives.lexing_make filename contents in
  let cst = Engine.parse false PrelexerState.initial_state lexbuf in
  cst.CST.value

let parse_file filename =
  let cin = open_in filename in
  let cst =
    try
      (** We assume that scripts are no longer than 16M. *)
      ExtPervasives.string_of_channel cin |> parse_string filename
    with e ->
      close_in cin;
      raise e
  in
  close_in cin;
  cst
