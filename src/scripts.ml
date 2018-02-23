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

open ExtPervasives
open ExtMenhirLib
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open MenhirLib.General
open CST
open Names
open Keywords
open Assignments
open Aliases
open Engine

let rec json_filter_positions =
  let open Yojson.Safe in
  function
  | `Assoc sjl ->
     if List.for_all (fun (s, j) -> s = "value" || s = "position") sjl then
       let (_, j) = List.find (fun (s, _) -> s = "value") sjl in
       json_filter_positions j
     else
       `Assoc (List.map (fun (s, j) ->
           Format.printf "%s@." s; (s, json_filter_positions j)) sjl
         )
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int i -> `Int i
  | `Intlit s -> `Intlit s
  | `List jl -> `List (List.map json_filter_positions jl)
  | `Null -> `Null
  | `String s -> `String s
  | `Tuple jl -> `Tuple (List.map json_filter_positions jl)
  | `Variant (s, None) -> `Variant (s, None)
  | `Variant (s, Some j) -> `Variant (s, Some (json_filter_positions j))

let save_as_json simplified cout csts =
  CSTHelpers.complete_command_list_to_json csts
  |> (if simplified then json_filter_positions else function x-> x)
  |> Yojson.Safe.pretty_to_channel cout

let other_scripts_magic_strings =
  List.map Str.regexp [
             "#![ ]*/usr/bin/perl.*";
             "#![ ]*/bin/bash.*"
           ]

let is_other_script filename =
  (* check whether [filename] is a script other than /bin/sh *)
  let cin = open_in filename in
  let firstline = input_line cin in
  close_in cin;
  List.exists
    (function r -> Str.string_match r firstline 0)
    other_scripts_magic_strings

let is_elf filename =
  (* check whether [filename] is an ELF executable *)
  let cin = open_in_bin filename
  and buf = Bytes.create 4 in
  let number_chars_read = input cin buf 0 4 in
  begin
    close_in cin;
    if number_chars_read < 4
    then false
    else (Bytes.compare buf (Bytes.of_string  "\x7FELF")) = 0
  end

let parse_file filename =
  (** We assume that scripts are no longer than 16M. *)
  let cin = open_in filename in
  let cst =
    try
      let contents = ExtPervasives.string_of_channel cin in
      let lexbuf = lexing_make filename contents in
      parse false [] lexbuf
    with e -> close_in cin;
              raise e
  in
  close_in cin;
  cst
