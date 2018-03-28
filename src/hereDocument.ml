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
open CST

module Lexer (U : sig end) : sig
  val push_here_document_delimiter : string -> unit
  val push_here_document_operator: bool -> (word located ref) -> unit
  val start_here_document_lexing: unit -> unit
  val next_here_document :
    Lexing.lexbuf -> Prelexer.pretoken * Lexing.position * Lexing.position
  val inside_here_document : unit -> bool
  val next_word_is_here_document_delimiter : unit -> bool
  val next_line_is_here_document: unit -> bool
end = struct

  (** specification:

      If more than one "<<" or "<<-" operator is specified on a line, the
      here-document associated with the first operator shall be supplied
      first by the application and shall be read first by the shell.

   *)
                      
  type delim_info = {
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
  let delimiters_queue = (Queue.create (): delim_info Queue.t)
  let dashed_tmp = ref (None: bool option)
  let word_ref_tmp = ref (None: word located ref option)

  type state =
    | NoHereDocuments
    (* we are currently not reading any here documents, nor have we seen
       a here document operator on the current line. *)
    | HereDocumentsStartOnNextLine
    (* we have seen a here document operator but we haven't yet finished
       the line, so reading of here documents has to start on the next line. *)
    | InsideHereDocuments
    (* we are currently in the process of reading here documents. *)
  let state = ref NoHereDocuments
  
  let push_here_document_operator dashed word_ref =
    assert (!state <> InsideHereDocuments);
    (* we accept a push of an operator only when the two variables 
       dashed_tmp and word_ref_tmp hold value None, that is either they
       have never been assigned a value, or they have been assigned a value
       which has been used up by push_here_document_delimiter.
     *)
    assert (!dashed_tmp = None);
    dashed_tmp := Some dashed;
    assert (!word_ref_tmp = None);
    word_ref_tmp := Some word_ref;
    state := HereDocumentsStartOnNextLine

  let push_here_document_delimiter w =
    (* we accept a push of a delimiting word only if we have already received
       information about an operator which has not yet been used.
     *)
    assert (!state <> InsideHereDocuments);
    let dashed = match !dashed_tmp with
      | Some b -> dashed_tmp:= None; b
      | None -> assert false
    and word_ref = match !word_ref_tmp with
      | Some r -> word_ref_tmp := None; r
      | None -> assert false
    and unquoted_w = QuoteRemoval.on_string w
    in
    Queue.add {
      (** specification:
          If any part of word is quoted, the delimiter shall be formed by
          performing quote removal on word, and the here-document lines shall
          not be expanded. Otherwise, the delimiter shall be the word itself.
       *)
        word = unquoted_w;
        quoted = (unquoted_w = w);
        dashed = dashed;
        contents_placeholder = word_ref
      } delimiters_queue

  let next_here_document lexbuf =
    (**specification:
       The here-document shall be treated as a single word that begins after
       the next <newline> and continues until there is a line containing only
       the delimiter and a <newline>, with no <blank> characters in
       between. Then the next here-document starts, if there is one.
     *)
    assert (!state = InsideHereDocuments);
    let delimiter_info = Queue.take delimiters_queue
    in
    let have_found_delimiter line =
      (* is [line] the current here-docuemnt delimiter ? *)
      let line = string_strip line
      in delimiter_info.word =
           (** specification:
               If the redirection operator is "<<-", all leading <tab>
                characters shall be stripped from ... the line containing
                the trailing delimiter.
            *)
           if delimiter_info.dashed
           then QuoteRemoval.remove_tabs_at_linestart line
           else line
    in
    let store_here_document contents doc_start doc_end  =
      (* store in the placeholder the here-document with contents [contents],
         start position [doc_start], and end position [doc_end]. *)
      let contents =
        (** specification:
          If no part of word is quoted ... the <backslash> in the
          input behaves as the <backslash> inside double-quotes (see
          Double-Quotes). However, the double-quote character ( ' )' shall
          not be treated specially within a here-document, except when the
          double-quote appears within "$()", "``", or "${}".
         *)
        if delimiter_info.quoted
        then QuoteRemoval.backslash_as_in_doublequotes contents
        else contents in
      let contents =
        (** specification:
           If the redirection operator is "<<-", all leading <tab>
           characters shall be stripped from input lines ...
         *)
        if delimiter_info.dashed
        then QuoteRemoval.remove_tabs_at_linestart contents
        else contents in
          delimiter_info.contents_placeholder :=
            CST.{
              value = Word.parse contents;
              position = { start_p = doc_start;
                           end_p = doc_end }
            }
    in
    let doc = Buffer.create 1000 in
    let firstline, doc_start, firstline_end =
      match Prelexer.readline lexbuf with
      | None -> failwith "Missing here document."
      | Some (l, b, e) -> l, b, e
    in
    let rec process line line_end =
      if have_found_delimiter line
      then
        begin
          store_here_document (Buffer.contents doc) doc_start line_end;
          if Queue.is_empty delimiters_queue then state := NoHereDocuments;
          let before_stop = Lexing.({ line_end with
                                      pos_cnum = line_end.pos_cnum - 1;
                                      pos_bol  = line_end.pos_bol  - 1;
                            }) in
          (Prelexer.NEWLINE, before_stop, line_end)
        end
      else
        begin
          Buffer.add_string doc line;
          match Prelexer.readline lexbuf with
          | None -> failwith "Unterminated here document."
          | Some (l,_,e) -> process l e
        end;
    in process firstline firstline_end

  let start_here_document_lexing () =
    assert (!state = HereDocumentsStartOnNextLine);
    state := InsideHereDocuments

  let next_word_is_here_document_delimiter () =
    (* if we have a value in dashed_tmp this means that we have read
       a here operator for which we have not yet seen the corresponding
       delimiting word.
          *)
    !dashed_tmp <> None
     
  let next_line_is_here_document () =
    !state = HereDocumentsStartOnNextLine

  let inside_here_document () =
    !state = InsideHereDocuments 

end
