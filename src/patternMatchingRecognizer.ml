(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017-2019 Yann Régis-Gianas, Nicolas Jeannerod,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

open REBracketExpressionParser
open REBracketExpressionParser.MenhirInterpreter
open CST

(** [re_bracket_lexing_mode] is used to interpret some special
    characters depending on the parsing context. *)

(** [recognize_re_bracket_expression s start] *)
let recognize_re_bracket_expression s start =

  let module Prelexer : sig
        val current_position : unit -> int
        val lexing_position : unit -> Lexing.position
        val next_token : unit -> token * Lexing.position * Lexing.position
        val after_starting_hat : unit -> bool
        val after_starting_bracket : unit -> bool
        val just_before_ending_bracket : unit -> bool
        val read_string : unit -> string
      end = struct

      let current_position = ref start

      let next_char () =
        if !current_position < String.length s then (
          let c = s.[!current_position] in
          incr current_position;
          Some c
        ) else None

      let eof_reached () =
        !current_position >= String.length s

      let lexbuf =
        Lexing.from_function @@ fun b count ->
          let rec aux i =
            if i = count then count
            else match next_char () with
                 | None -> i
                 | Some c -> Bytes.set b i c; aux (i + 1)
          in
          aux 0

      let lookahead_buffer = Queue.create ()

      let with_positions token =
        (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

      let lex () =
        REBracketExpressionLexer.token lexbuf |> with_positions

      let next_token () =
        if Queue.is_empty lookahead_buffer then
          lex ()
        else if eof_reached () then
          with_positions REBracketExpressionParser.EOF
        else
          Queue.pop lookahead_buffer

      let read_string () =
        String.sub s start (!current_position - start)

      let current_position () =
        lexbuf.Lexing.lex_start_p.pos_cnum

      let lexing_position () = lexbuf.Lexing.lex_start_p

      let after_starting_bracket () =
        (lexing_position ()).pos_cnum = 1

      let just_before_ending_bracket () =
        (lexing_position ()).pos_cnum = String.length s - 2 - start

      let after_starting_hat () =
        (lexing_position ()).pos_cnum = 2 && s.[1] = '^'

    end
  in
  (*specification:

    META_CHAR One of the characters:

      ^
      When found first in a bracket expression

      -
      When found anywhere but first (after an initial '^', if any) or last in a
      bracket expression, or as the ending range point in a range expression

      ]
      When found anywhere but first (after an initial '^', if any) in a
      bracket expression

   *)
  (**
      The specification phrasing is a bit ackward. We interpret it as a
      definition for tokens HAT, MINUS and RBRACKET as well a definition
      for META_CHAR which is supposed to be the union of these three tokens.

      As a consequence, META_CHAR is better expressed as a non terminal
      `meta_char`. We decided to slightly change the POSIX standard grammar
      in that direction because it seems to make more sense.
  *)
  let next_token () =
    let rewrite_token f =
      let (token, p1, p2) = Prelexer.next_token () in
      (f token, p1, p2)
    in
    rewrite_token (function
      | HAT ->
         if Prelexer.after_starting_bracket () then
           HAT
         else
           (* by 2.13.1, <circumflex> is <exclamation-mark> in the context of
              shell. *)
           COLL_ELEM_SINGLE '!'
      | (RBRACKET | MINUS) as token ->
         let final_minus =
           (token = MINUS) && (Prelexer.just_before_ending_bracket ())
         in
         (** The case of MINUS is a bit subtle: we only have to handle the
             first subcase (being at the first position or after the starting
             hat) because the grammar is handling the next two subcases. *)
         if Prelexer.(after_starting_bracket ()
                      || after_starting_hat ()
                      || final_minus)
         then
           COLL_ELEM_SINGLE (if token = MINUS then '-' else ']')
         else
           token
      | token ->
         token)
  in
  let rec parse checkpoint =
    match checkpoint with
    | InputNeeded _ ->
       parse (offer checkpoint (next_token ()))
    | Accepted cst ->
       Some (cst, Prelexer.read_string (), Prelexer.current_position ())
    | Rejected ->
       None
    | _ ->
       parse (resume checkpoint)
  in
  parse (Incremental.bracket_expression (Prelexer.lexing_position ()))

(** [process s] recognizes pattern-matching expressions inside [s]
    returning the resulting stream of word components, in reverse
    order. *)
let process s : (string * word_component) list =
  let b = Buffer.create 31 in
  let rec analyze output i =
    let flush () =
      if Buffer.length b > 0 then
        let w = Buffer.contents b in
        (w, WordLiteral w) :: output
      else
        output
    in
    let produce ?(next=i+1) char ast =
      Buffer.clear b;
      analyze ((char, ast) :: flush ()) next
    in
    let push char =
      Buffer.add_char b char;
      analyze output (i + 1)
    in
    if i >= String.length s then
      flush ()
    else
      match s.[i] with
      | '?' -> produce "?" WordGlobAny
      | '*' -> produce "*" WordGlobAll
      | '[' -> begin match recognize_re_bracket_expression s i with
               | Some (re_bracket_exp, rs, j) ->
                  produce rs (WordReBracketExpression re_bracket_exp) ~next:j
               | None ->
                  push '['
               end
      | c -> push c
  in
  analyze [] 0
