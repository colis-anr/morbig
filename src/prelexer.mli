(**

   This module implements the token recognizer when it is not in the mode
   that recognizes here-documents, and in cases where the recognition of
   tokens is independent from the parsing context, as specified by:

              http://pubs.opengroup.org/onlinepubs/9699919799/
              2.3 Token Recognition

*)

type pretoken =
  | Word of string (* never an empty string *)
  | Operator of Parser.token
  | EOF
  | NEWLINE

(** [token b l] advances in the lexbuf [l], and produces a list of
    pretokens from [l], using a queue of symbols in the buffer [b] *)
(* FIXME: we probably have that [b] is always empty when [token] is called *)
val token :
  Buffer.t -> Lexing.lexbuf ->
  (pretoken * Lexing.position * Lexing.position) list

(** [readline l] returns [None] when the lexbuf is at the end of input,
    or otherwise [Some (l,pstart,pstop)] where [l] is the next line read
    from the lexbuf (including the terminating <newline> when present),
    and pstart and pstop are the start and end position of this line.
 *)
val readline :
  Lexing.lexbuf -> (string  * Lexing.position * Lexing.position) option
