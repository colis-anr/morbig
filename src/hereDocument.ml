(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Nicolas Jeannerod,    *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

open CST
open PrelexerState

module Lexer (U : sig end) : sig
  val push_here_document_delimiter : string -> word_cst -> unit
  val push_here_document_operator: bool -> (word located ref) -> unit
  val start_here_document_lexing: unit -> unit
  val next_here_document :
    Lexing.lexbuf -> PrelexerState.t
    -> Pretoken.t * Lexing.position * Lexing.position
  val inside_here_document : unit -> bool
  val next_word_is_here_document_delimiter : unit -> bool
  val next_line_is_here_document: unit -> bool
end = struct

  (*specification:

      If more than one "<<" or "<<-" operator is specified on a line, the
      here-document associated with the first operator shall be supplied
      first by the application and shall be read first by the shell.

   *)

  type delimiter_info = {
        (** information about a delimiter of a here document: *)
      word: string;
        (** delimiting word, with quotes removed *)
      quoted: bool;
        (** parts of delimiting word quoted ? *)
      dashed: bool;
        (** here operator <<- ? *)
      contents_placeholder: CST.word CST.located ref
        (** placeholder for the contents of the here document *)
    }
  let delimiters_queue = (Queue.create (): delimiter_info Queue.t)
  let dashed_tmp = ref (None: bool option)
  let word_ref_tmp = ref (None: word located ref option)

  type state =
    | NoHereDocuments
    (* we are currently not reading any here documents, nor have we seen
       a here document operator on the current line. *)
    | GotHereOperator
    (* we have seen a here document operator but we haven't seen the 
       corresponding delimite word yet.  *)
    | GotDelimiter
    (* we have seen a here document operator and its delimiter word. *)
    | InsideHereDocuments
    (* we are currently in the process of reading here documents. *)
  let state = ref NoHereDocuments

  let push_here_document_operator dashed word_ref =
    if !state = GotHereOperator then
      (* FIXME: we should raise an Error.DuringParsing here if we can
         get the current lexing position. *)
      failwith "redirection operator found where a delimter word is expected";
    assert (!state = NoHereDocuments || !state = GotDelimiter);
    (* we accept a push of an operator only when the two variables
       dashed_tmp and word_ref_tmp hold value None, that is either
       - they have not been assigned a value (state NoHereDocuments),
       - or they have been assigned a value which has been used up by
         push_here_document_delimiter (state GotDelimiter).
     *)
    assert (!dashed_tmp = None);
    dashed_tmp := Some dashed;
    assert (!word_ref_tmp = None);
    word_ref_tmp := Some word_ref;
    state := GotHereOperator

  let push_here_document_delimiter _w cst =
    (* we accept a push of a delimiting word only if we have already received
       information about an operator which has not yet been used.
     *)
    assert (!state = GotHereOperator);
    let quoted_flag = ref false in
    let dashed = match !dashed_tmp with
      | Some b -> dashed_tmp:= None; b
      | None -> assert false
    and word_ref = match !word_ref_tmp with
      | Some r -> word_ref_tmp := None; r
      | None -> assert false
    and unquoted_w =
      let unword (Word (s, _)) = s in
      let rec unquote = function
        | [] -> ""
        | WordDoubleQuoted s :: w ->
           QuoteRemoval.on_string (unword s) ^ unquote w
        | WordSingleQuoted s :: w ->
           unword s ^ unquote w
        | (WordLiteral s | WordName s) :: w ->
           let s' = Str.(global_replace (regexp "\\") "" s) in
           if s <> s' then quoted_flag := true;
           s' ^ unquote w
        | WordVariable (VariableAtom (s, NoAttribute)) :: w ->
           "$" ^ s ^ unquote w
        | _ ->
           failwith "Unsupported expansion in here document delimiter"
      in
      unquote cst
    in
    let quoted =
      !quoted_flag
      || List.exists (function WordSingleQuoted _ -> true | _ -> false) cst

    in
    Queue.add {
      (*specification:
          If any part of word is quoted, the delimiter shall be formed by
          performing quote removal on word, and the here-document lines shall
          not be expanded. Otherwise, the delimiter shall be the word itself.
       *)
        word = unquoted_w;
        quoted;
        dashed;
        contents_placeholder = word_ref
      } delimiters_queue;
    state := GotDelimiter

  let next_here_document lexbuf current =
    (*specification:
       The here-document shall be treated as a single word that begins after
       the next <newline> and continues until there is a line containing only
       the delimiter and a <newline>, with no <blank> characters in
       between. Then the next here-document starts, if there is one.
     *)
    assert (!state = InsideHereDocuments);
    let delimiter_info =
      try
        Queue.take delimiters_queue
      with Queue.Empty -> failwith "here document problem"
    in

    let store_here_document end_marker cst contents doc_start doc_end =
      (* store in the placeholder the here-document with contents [contents],
         start position [doc_start], and end position [doc_end]. *)
      (*specification:
          If no part of word is quoted ... the <backslash> in the
          input behaves as the <backslash> inside double-quotes (see
          Double-Quotes). However, the double-quote character ( ' )' shall
          not be treated specially within a here-document, except when the
          double-quote appears within "$()", "``", or "${}".
      *)
      let contents =
        if delimiter_info.quoted then
          QuoteRemoval.backslash_as_in_doublequotes contents
        else
          contents
      in
      let contents, cst =
        remove_contents_suffix doc_end end_marker contents cst
      in
      let contents =
        (*specification:
            If the redirection operator is "<<-", all leading <tab>
            characters shall be stripped from input lines ...
         *)
        if delimiter_info.dashed then
          QuoteRemoval.remove_tabs_at_linestart contents
        else
          contents
      in
      delimiter_info.contents_placeholder :=
        CST.{
          value = Word (contents, cst);
          position = { start_p = doc_start; end_p = doc_end }
        }
    in
    let ((Word (doc, cst)), doc_start, line_end) =
      let current =
        enter_here_document delimiter_info.dashed delimiter_info.word current
      in
      let result =
        if delimiter_info.quoted then
          let buffer = Buffer.create 13 in
          let current = Prelexer.single_quotes buffer current lexbuf in
          return lexbuf current []
        else
          Prelexer.token current lexbuf
      in
      match result with
      | [Pretoken.NEWLINE, p1, p2] ->
         (* Special case for empty here document or ended by EOF. *)
         (Word ("", []), p1, p2)
      | [Pretoken.EOF, _, pos] ->
         raise (Errors.DuringParsing pos)
      | result ->
         located_word_of result
    in
    store_here_document delimiter_info.word cst doc doc_start line_end;
    if Queue.is_empty delimiters_queue then state := NoHereDocuments;
    let before_stop =
      Lexing.({ line_end with
                pos_cnum = line_end.pos_cnum - 1;
                pos_bol  = line_end.pos_bol  - 1;
      })
    in
    (Pretoken.NEWLINE, before_stop, line_end)

  let start_here_document_lexing () =
    assert (!state = GotDelimiter);
    state := InsideHereDocuments

  let next_word_is_here_document_delimiter () =
    (* if we have a value in dashed_tmp this means that we have read
       a here operator for which we have not yet seen the corresponding
       delimiting word.
          *)
    !dashed_tmp <> None

  let next_line_is_here_document () =
    !state = GotDelimiter

  let inside_here_document () =
    !state = InsideHereDocuments

end
