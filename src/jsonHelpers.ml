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

let rec json_filter_positions = function
  | `Assoc sjl ->
     if List.for_all (fun (s, _j) -> s = "value" || s = "position") sjl then
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

let convert_to_json simplified csts =
  CSTHelpers.program_to_json csts
  |> (if simplified then json_filter_positions else function x-> x)

let save_as_json simplified cout csts =
  convert_to_json simplified csts
  |> Yojson.Safe.pretty_to_channel cout

let load_from_json cin =
  Yojson.Safe.from_channel cin |> CST.program_of_yojson
  |> Ppx_deriving_yojson_runtime.Result.(function
    | Ok cst -> cst
    | Error msg -> raise (Errors.DuringIO msg)
  )

let json_to_dot cout json =
  Printf.(
    let fresh =
      let r = ref 0 in
      fun () ->
        incr r;
        Printf.sprintf "node%d" !r
    in
    let rec traverse = function
      | `List (`String name :: children) ->
         let nodeid = fresh () in
         fprintf cout "%s [label=\"%s\"];\n" nodeid name;
         let childrenids = List.map traverse children in
         List.iter (fun c -> fprintf cout "%s -> %s;\n" nodeid c) childrenids;
         nodeid
      | `String name ->
         let nodeid = fresh () in
         fprintf cout "%s [label=\"%s\"];\n" nodeid (String.escaped name);
         nodeid
      | `List [x] ->
         traverse x
      | `List children ->
         let nodeid = fresh () in
         fprintf cout "%s [shape=point];\n" nodeid;
         let childrenids = List.map traverse children in
         List.iter (fun c -> fprintf cout "%s -> %s;\n" nodeid c) childrenids;
         nodeid
      | _ ->
         assert false
    in
    fprintf cout "digraph {\n";
    ignore (traverse json);
    fprintf cout "}\n"
  )

let save_as_dot cout csts =
  convert_to_json true csts
  |> json_to_dot cout
