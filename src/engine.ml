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

(** Raise in case of parsing error. *)
exception ParseError

type 'a state = {
    checkpoint : 'a checkpoint;
    aliases    : Aliases.t;
  }

module type Lexer =
  sig
    val initialize : Lexing.lexbuf -> unit
    val next_token : 'a state -> token * Lexing.position * Lexing.position
    val eof : bool ref
    val real_eof : bool ref
    val tokens : token list ref
    val current_position : unit -> Lexing.position
  end

let parse partial (module Lexer : Lexer) =

    (**--------------**)
    (** Parsing loop. *)
    (**--------------**)

  let rec parse { aliases; checkpoint } =
    match checkpoint with
      (**

         If the parser requires some extra input to continue
         the analyze, [next_token] is called with the current
         parsing state as argument.

      *)
      | InputNeeded parsing_state ->
        let (token, ps, pe) as input = Lexer.next_token { aliases; checkpoint } in
        parse { aliases; checkpoint = offer checkpoint (token, ps, pe) }

    (**

       If the parser has recognized a complete command and
       we are not at the end of the input, we restart a parser
       on the sequel.

    *)
      | Accepted cst ->
        let aliases = Aliases.interpret aliases cst in
        Lexer.eof := false;
        if !Lexer.real_eof || partial then
          [cst]
        else (
          Lexer.tokens := [];
          let checkpoint = complete_command (Lexer.current_position ()) in
          cst :: parse { aliases; checkpoint }
        )

    (**

       The parser has rejected the input.

    *)
      (* FIXME: Generate a better error message. *)
      | Rejected ->
        (**

           We want to recognize a *prefix* of the input stream.

           Therefore, if a token produces a parse error, it might be
           possible that the currently read prefix of the input
           already is a valid shell script. To check that, we roll
           back to the previous state and we inject EOF to check if
           the fragment of the input already read can be recognized as
           a complete command.

         *)
         if !Lexer.tokens = [EOF] then
           []
         else
           (* FIXME: To be reimplemented. *)
           raise ParseError

      (**

         The specification grammar has a minor incompleteness problem:
         the start symbol should have an extra rule to accept an empty
         input.

         To deal with this incompleteness of the grammar, we
         detect parsing errors that are raised when an empty input is
         provided to the parser. In that case, we simply accept the
         program.

         FIXME: Is that clear that we do not introduce more scripts in
         the language?

    *)

      | HandlingError env ->
         if MenhirInterpreter.current_state_number env = 0
            && !Lexer.tokens = [EOF]
         then
           []
         else
           parse { aliases; checkpoint = resume checkpoint }

      (**

         The shell grammar follows a parsing-dependent lexical
         analysis: they are some places where a reserved word must be
         recognized as a simple word when it cannot be written at a
         given place of the input (see
         [recognize_reserved_word_if_relevant] defined
         earlier). However, they are some other places where this
         conversion from reserved words to simple words is forbidden.

         For instance, while the input

         `` echo else ``

         is syntactically correct, the input

         `` else echo ``

         is not.

         Instead of complicating
         [recognize_reserved_word_if_relevant], we decided to detect a
         posteriori when the conversion from reserved words to simple
         words should not have been made. This detection is easily
         feasible because there is actually only one place in the
         grammar where this conversion is forbidden: a reserved word
         can never be converted to a simple word where a [cmd_word] is
         expected.

         Fortunately, menhir gives us the control back when it is
         about to reduce a nonterminal. Therefore, it is possible to
         detect when a simple word, which is also a reserved word, has
         been reduced to a [cmd_word].

      *)
      | AboutToReduce (env, production) ->
        begin try
          if lhs production = X (N N_cmd_word)
          || lhs production = X (N N_cmd_name) then
            match top env with
            | Some (Element (state, v, _, _)) ->
              let analyse_top : type a. a symbol * a -> _ = function
                | T T_NAME, Name w when is_reserved_word w -> raise ParseError
                | T T_WORD, Word w when is_reserved_word w -> raise ParseError
                | _ ->
                  (* By correctness of the underlying LR automaton. *)
                  raise Not_found
              in
              analyse_top (incoming_symbol state, v)
            | _ ->
              (* By correction of the underlying LR automaton. *)
              assert false
          else raise Not_found
        with Not_found ->
          parse { aliases; checkpoint = resume checkpoint }
      end

    (**

       The other intermediate steps of the parser are ignored.

    *)

      | Shifting (_, _, _) ->
        parse { aliases; checkpoint = resume checkpoint }

  in
  parse {
      aliases = Aliases.empty;
      checkpoint = complete_command (Lexer.current_position ())
    }

module Lexer : Lexer = struct

  (**--------------------------**)
  (** {!Prelexer} pretokenizer. *)
  (**--------------------------**)

  let next_pretoken = ref (fun () -> assert false)

  let push_pretoken = ref (fun token -> assert false)

  let global_lexbuf = ref None

  exception UninitializeLexer

  let lexbuf () =
    match !global_lexbuf with 
    | None -> raise UninitializeLexer
    | Some lexbuf -> lexbuf


  let initialize lexbuf =
    let _next_pretoken, _push_pretoken = Pretokenizer.make lexbuf in
    next_pretoken := _next_pretoken;
    push_pretoken := _push_pretoken

  (**---------------------**)
  (** Parsing-aware lexer. *)
  (**---------------------**)

  (** Once end-of-command has been reached, the lexer must return an
      end-of-file token each time it is subsequently called. The
      following boolean accounts for this two-states mechanism. *)
  let eof = ref false
  let real_eof = ref false

  (** The lexer works in two modes: either it is recognizing a
      here-document, or it is recognizing tokens as defined in
      the shell grammar. *)
  module HDL = HereDocument.Lexer (struct end)

  let tokens = ref []

  let rec next_token { aliases; checkpoint } =
    if HDL.inside_here_document () then (
      !push_pretoken (HDL.next_here_document (lexbuf ()));
      next_token { aliases; checkpoint }
    )
    else
      let (pretoken, pstart, pstop) as p = !next_pretoken () in
      let return token =
        if token = EOF then eof := true;
        let token = if !eof then EOF else token in
        (token, pstart, pstop)
      in
      match pretoken with
        | Prelexer.IoNumber i ->
          return (IO_NUMBER (IONumber i))

        | Prelexer.Word w ->

        (**specification

           [Command Name]

           When the TOKEN is exactly a reserved word, the token
           identifier for that reserved word shall result. Otherwise,
           the token WORD shall be returned. Also, if the parser is in
           any state where only a reserved word could be the next
           correct token, proceed as above.

           Note: Because at this point <quotation-mark> characters are
           retained in the token, quoted strings cannot be recognized
           as reserved words. This rule also implies that reserved
           words are not recognized except in certain positions in the
           input, such as after a <newline> or <semicolon>; the
           grammar presumes that if the reserved word is intended, it
           is properly delimited by the user, and does not attempt to
           reflect that requirement directly. Also note that line
           joining is done before tokenization, as described in Escape
           Character (Backslash), so escaped <newline> characters are
           already removed at this point.  Rule 1 is not directly
           referenced in the grammar, but is referred to by other
           rules, or applies globally.

        *)
          let w = alias_substitution aliases checkpoint w in

          let token = FirstSuccessMonad.(
            (recognize_assignment checkpoint p w)
            +> (recognize_reserved_word_if_relevant checkpoint p w)
            +> return (WORD (Word w))
          )
          in
          if HDL.next_word_is_here_document_delimiter () then
            (**specification

                2.7.4 Here-Document

                If any part of word is quoted, the delimiter shall be
                formed by performing quote removal on word, and the
                here-document lines shall not be expanded. Otherwise,
                the delimiter shall be the word itself.

            *)
            HDL.push_next_word_as_here_document_delimiter w;
          return (FirstSuccessMonad.should_succeed token)

        | Prelexer.EOF ->
          real_eof := true;
          return EOF

        | Prelexer.Operator ((DLESS r | DLESSDASH r) as token) ->
          let dashed = match token with DLESSDASH _ -> true | _ -> false in
          HDL.here_document_lexing_on_next_line dashed r;
          return token

        | Prelexer.Operator token ->
          return token

        | Prelexer.NEWLINE ->
        (** The interpretation of the pretoken [NEWLINE] depends
            on the parsing context: *)

        (** If we are to recognize a here-document, [NEWLINE] triggers
            the here-document lexing mode. *)
          if HDL.next_line_is_here_document () then (
            HDL.start_here_document_lexing ();
            next_token { aliases; checkpoint }
          )

        (** If the input is completed, [NEWLINE] is interpreted
            as the end-of-file marker. *)
          else if finished (offer checkpoint (EOF, pstart, pstop)) then (
            return EOF
          )

        (** If the input is not completed but [NEWLINE] as a meaning
            from the point of view of the grammar, it is promoted as a
            token and communicated to the parser. *)
          else if accepted_token checkpoint (NEWLINE, pstart, pstop) then
            return NEWLINE

        (** Otherwise, a [NEWLINE] is simply layout and is ignored. *)
          else next_token { aliases; checkpoint }

  let next_token { aliases; checkpoint } =
    let (raw, _, _) as token = next_token { aliases; checkpoint } in
    tokens := raw :: !tokens;
    token

  let current_position () =
    (lexbuf ()).Lexing.lex_curr_p

end

let parse partial lexbuf =
  Lexer.initialize lexbuf;
  parse partial (module Lexer)

let close_knot = RecursiveParser.parse := (parse true)
