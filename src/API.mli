(** This interface defines the API of libmorbig *)

(** Raised in case of syntax error with a message. *)
exception SyntaxError of CST.position * string

(** [parse_file filename] performs the syntactic analysis of
   [filename] and returns a concrete syntax tree if [filename] content
   is syntactically correct. Raise {SyntaxError (pos, msg)}
   otherwise. *)
val parse_file : string -> CST.complete_command_list
