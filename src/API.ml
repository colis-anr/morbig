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

exception SyntaxError of CST.position * string

let parse_file = Scripts.parse_file

(* Called from C stub. *)

type ccst =
  | Location of CST.lexing_position * CST.lexing_position * ccst
  | Node     of string * ccst array
  | Data     of string

let ccst_of_json_program j =
  let unexpected_case json =
    Printf.eprintf "Unexpected json: %s\n" (Yojson.Safe.pretty_to_string json);
    Printexc.print_backtrace stderr;
    exit 1
  in
  let rec aux = function
    | `Assoc [ "value", v; "position", p ] ->
       let start_p, end_p = location p in
       Location (start_p, end_p, aux v)
    | `List (`String k :: children) ->
       Node (k, aux' (`List children))
    | `Variant (k, None) ->
       Node (k, [||])
    | `Variant (k, Some children) ->
       Node (k, aux' children)
    | `String s ->
       Data s
    | `List l ->
       Node ("Tuple", aux' (`List l))
    | json ->
       unexpected_case json
  and aux' = function
    | `List c ->
       Array.of_list (List.map aux c)
    | `Assoc m ->
       aux' (`List (snd (List.split m)))
    | json ->
       unexpected_case json
  and position = function
    | `Assoc [ "pos_fname", `String pos_fname;
               "pos_lnum", `Int pos_lnum;
               "pos_bol", `Int pos_bol;
               "pos_cnum", `Int pos_cnum ] ->
       CST.({ pos_fname; pos_lnum; pos_bol; pos_cnum })
    | json ->
       unexpected_case json
  and location = function
    | `Assoc [ "start_p", start_p; "end_p", end_p ] ->
       (position start_p, position end_p)
    | json ->
       unexpected_case json
  in
  aux j

let ccst_roots = ref []

let register cst =
  ccst_roots := cst :: !ccst_roots;
  cst

exception CSTDisposalFailed

let dispose_cst cst =
  if List.memq cst !ccst_roots then
    ccst_roots := List.filter (( == ) cst) !ccst_roots
  else
    raise CSTDisposalFailed

let untyped_parse_file s =
  parse_file s |> CSTHelpers.program_to_json |> ccst_of_json_program |> register

let _ =
  Callback.register "untyped_parse_file" untyped_parse_file;
  Callback.register "dispose_cst" dispose_cst
