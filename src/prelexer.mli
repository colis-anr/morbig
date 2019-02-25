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

(**

   This module implements the token recognizer when it is not in the mode
   that recognizes here-documents, and in cases where the recognition of
   tokens is independent from the parsing context, as specified by:

              http://pubs.opengroup.org/onlinepubs/9699919799/
              2.3 Token Recognition

*)

(** [token b l] advances in the lexbuf [l], and produces a list of
    pretokens from [l], using a queue of symbols in the buffer [b] *)
val token :
  PrelexerState.t ->
  Lexing.lexbuf ->
  (Pretoken.t * Lexing.position * Lexing.position) list

(** [single_quotes b l] advances in the lexing buffer [l] to
    recognize a word between single quotes. This is used by
    here-document recognition. *)
val single_quotes :
  PrelexerState.t -> Lexing.lexbuf -> PrelexerState.t
