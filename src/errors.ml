(** Raise in case of parsing error. *)
exception ParseError of Lexing.position

(** Raise in case of parsing error. *)
exception LexicalError of Lexing.position * string
