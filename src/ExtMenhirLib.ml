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

let is_accepted_token checkpoint token =
  accepted_token checkpoint token <> Wrong

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

(** [nonterminal_production p] returns the non terminal of [p].
    The nonterminals of Menhir API are a too precisely typed for
    our needs. Hence, we introduce an extential type for weaken
    this precision. *)
type nonterminal =
  AnyN : 'a Parser.MenhirInterpreter.nonterminal -> nonterminal

let nonterminal_of_production p =
  match lhs p with
  | X (N nt) -> AnyN nt
  | _ -> assert false (* Because every production as a nonterminal. *)

exception EmptyStack

type 'b top_symbol_processor = {
   perform : 'a. 'a symbol * 'a -> 'b
}

let on_top_symbol env f =
  match top env with
  | Some (Element (state, v, _, _)) -> f.perform (incoming_symbol state, v)
  | _ -> raise EmptyStack
