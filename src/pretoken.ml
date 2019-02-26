open Parser

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

let operators = Hashtbl.(
    let t = create 17 in
    List.iter (fun (sym, tok) -> add t sym tok) [
        "&&",  AND_IF;
        "||", OR_IF;
        ";;", DSEMI;
        "<&", LESSAND;
        ">&", GREATAND;
        "<>", LESSGREAT;
        ">>", DGREAT;
        ">|", CLOBBER;
        "|",  Pipe;
        "(",  Lparen;
        ")",  Rparen;
        "<",  LESS;
        ">",  GREAT;
        ";",  Semicolon;
        "&",  Uppersand
      ];
    t
 )

let optoken_of_string s =
  try
    Operator (Hashtbl.find operators s)
  with Not_found ->
    Printf.eprintf
      "Internal error: `%s' is not a valid operator token.\n"
       s;
    assert false

let preword_of_operator = function
    | AND_IF -> "&&"
    | OR_IF -> "||"
    | DSEMI -> ";;"
    | LESSAND -> "<&"
    | GREATAND -> ">&"
    | LESSGREAT -> "<>"
    | DGREAT -> ">>"
    | CLOBBER -> ">|"
    | Pipe -> "|"
    | Lparen -> "("
    | Rparen -> ")"
    | LESS -> "<"
    | GREAT -> ">"
    | Semicolon -> ";"
    | Uppersand -> "&"
    | _ -> assert false (* By definition of operators. *)

let preword_of_pretoken = function
  | IoNumber s -> PreWord (s, [WordLiteral s])
  | Operator o -> let s = preword_of_operator o in PreWord (s, [WordLiteral s])
  | NEWLINE -> PreWord ("\n", [WordLiteral "\n"])
  | p -> p
