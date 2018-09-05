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

(** HereDocument.Lexer handles here documents. It does:
    - internal registration of redirection operators (<< or <<-), and
      of the delimiter word that follows such an operator.
    - scannig of the contents of a series of here documents.
 *)

module Lexer :
  functor (U : sig  end) ->
    sig
      val push_here_document_operator :
        bool -> CST.word CST.located ref -> unit
      (** [push_here_document_operator dashed word_ref] registers a redirection
          operator:
          - [dashed] is [true] when the operator is <<-, and [false] if <<-
          - [word_ref] is a reference to a located word. This reference will
             later be assigned the contents of the here document.
       *)
      val push_here_document_delimiter : string -> CST.word_cst -> unit
      (** [push_here_document_delimiter word] registers [word] as the
          delimiting word pertaining to the preceding redirection operator.
       *)

      val start_here_document_lexing : unit -> unit
      (** start scanning the here documents that we have registered. *)
      val next_here_document :
        Lexing.lexbuf -> PrelexerState.t ->
        Pretoken.t * Lexing.position * Lexing.position
      (** scans the contents of a here document including the line containing
          the delimiter. Returns the pretoken containing the contents of the
          here document. As a side effect, assigns the contents of the here
          document to the reference that was registered by
          [push_here_document_operator].
       *)

      val inside_here_document : unit -> bool
      (** Are we currently reading a sequence of here documents? *)
      val next_word_is_here_document_delimiter : unit -> bool
      (** Must the next word be a here document delimiter? *)
      val next_line_is_here_document : unit -> bool
      (** Do we have to read here documents starting from the next line? *)
    end
