(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017 Yann Régis-Gianas, Nicolas Jeannerod,             *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open MenhirLib.General

let current_items parsing_state =
  match Lazy.force (stack parsing_state) with
    | Nil ->
      []
    | Cons (Element (s, _, _, _), _) ->
      items s

let rec close checkpoint =
  match checkpoint with
    | AboutToReduce (_, _) -> close (resume checkpoint)
    | Rejected | HandlingError _ -> false
    | Accepted _ | InputNeeded _ | Shifting _ -> true

let accepted_token checkpoint token =
  match checkpoint with
    | InputNeeded _ -> close (offer checkpoint token)
    | _ -> false

(** [finished checkpoint] is [true] if the current [checkpoint] can
    move the LR(1) automaton to an accepting state with no extra
    input.
*)
let rec finished = function
  | Accepted _ -> true
  | (AboutToReduce (_, _) | Shifting (_, _, _)) as checkpoint ->
    finished (resume checkpoint)
  | _ -> false
