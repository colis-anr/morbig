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

type 'a status =
  | AcceptedNow of 'a
  | Fine
  | Wrong

let rec close checkpoint =
  match checkpoint with
    | AboutToReduce (_, _) | Shifting _ -> close (resume checkpoint)
    | Rejected | HandlingError _ -> Wrong
    | Accepted x -> AcceptedNow x
    | InputNeeded _ -> Fine

let accepted_token checkpoint token =
  match checkpoint with
    | InputNeeded _ -> close (offer checkpoint token)
    | _ -> Wrong

let accepted_raw_token checkpoint raw_token =
  accepted_token checkpoint (raw_token, Lexing.dummy_pos, Lexing.dummy_pos)

(** [finished checkpoint] is [true] if the current [checkpoint] can
    move the LR(1) automaton to an accepting state with no extra
    input.
*)
let rec finished = function
  | Accepted _ -> true
  | (AboutToReduce (_, _) | Shifting (_, _, _)) as checkpoint ->
    finished (resume checkpoint)
  | _ -> false
