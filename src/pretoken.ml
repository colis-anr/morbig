type t =
  | PreWord of string * CST.word_cst
  | IoNumber of string
  | Operator of Parser.token
  | EOF
  | NEWLINE

let string_of_pretoken = function
  | PreWord (s, _) -> Printf.sprintf "PREWORD(%s)" s
  | IoNumber s -> Printf.sprintf "IONUM(%s)" s
  | Operator t -> Printf.sprintf "OPERATOR(%s)" (Token.string_of_token t)
  | EOF -> "EOF"
  | NEWLINE -> "NEWLINE"
