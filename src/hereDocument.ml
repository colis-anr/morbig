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
open CST
open Prelexer
open Lexing

module Lexer (U : sig end) : sig
  val fill_next_here_document_placeholder : word located -> unit
  val inside_here_document : unit -> bool
  val next_here_document : lexbuf -> pretoken * position * position
  val next_word_is_here_document_delimiter : unit -> bool
  val push_next_word_as_here_document_delimiter : string -> unit
  val here_document_lexing_on_next_line: bool -> (word located ref) -> unit
  val next_line_is_here_document: unit -> bool
  val start_here_document_lexing: unit -> unit
end = struct

  let on_next_line   = ref false
  let lexing         = ref false
  let delimiters     = ref ([] : string list)
  let skip_tabs      = ref ([] : bool list)
  let find_delimiter = ref false
  let placeholders   = ref ([] : word located ref list)

  let fill_next_here_document_placeholder here_document =
    assert (!placeholders <> []);
    (List.hd !placeholders) := here_document;
    placeholders := List.tl !placeholders

  let next_here_document lexbuf =
    assert (!delimiters <> []);
    assert (!skip_tabs <> []);
    let delimiter = List.hd !delimiters
    and skip_tabs_flag = List.hd !skip_tabs
    and doc = Buffer.create 1000
    and nextline, pstart, pstop =
      match Prelexer.readline lexbuf with
        | None -> failwith "Unterminated here document."
        | Some (l, b, e) -> (ref l, ref b, ref e)
    in
    while (string_strip (
               if skip_tabs_flag then string_untab !nextline else !nextline
             ) <> delimiter)
    do
      Buffer.add_string doc !nextline;
      match Prelexer.readline lexbuf with
        | None -> failwith "Unterminated here document."
        | Some (l,b,e) -> nextline := l;
          pstop := e
    done;
    delimiters := List.tl !delimiters;
    skip_tabs := List.tl !skip_tabs;
    if !delimiters = [] then lexing := false;
    let before_stop = Lexing.({ !pstop with
      pos_cnum = !pstop.pos_cnum - 1;
      pos_bol  = !pstop.pos_bol  - 1;
    }) in
    fill_next_here_document_placeholder (CST.{
        value = Word (Buffer.contents doc);
        position = { start_p = CSTHelpers.internalize !pstart;
                     end_p = CSTHelpers.internalize !pstop }
    });
    (Prelexer.NEWLINE, before_stop, !pstop)

  let here_document_lexing_on_next_line dashed r =
    on_next_line := true;
    find_delimiter := true;
    placeholders := r :: !placeholders;
    skip_tabs := dashed :: !skip_tabs

  let next_word_is_here_document_delimiter () =
    !find_delimiter

  let next_line_is_here_document () =
    !on_next_line

  let start_here_document_lexing () =
    on_next_line := false;
    lexing := true;
    delimiters := List.rev !delimiters;
    skip_tabs := List.rev !skip_tabs;
    placeholders := List.rev !placeholders

  let push_next_word_as_here_document_delimiter w =
    delimiters := (QuoteRemoval.on_string w) :: !delimiters;
    find_delimiter := false

  let inside_here_document () =
    !lexing

end
