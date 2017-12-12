open CST
open CSTHelpers
open Parser

let string_of_hd hd = unWord hd.value

let string_of_io_number (IONumber io) = io

let string_of_assignment_word aw = unWord (word_of_assignment_word aw)

let string_of_token = function
  | EOF -> "EOF"
  | AND_IF -> "AND_IF"
  | OR_IF -> "OR_IF"
  | DSEMI -> "DSEMI"
  | DLESS wr -> Printf.sprintf "DLESS(%s)" (string_of_hd !wr)
  | DLESSDASH wr -> Printf.sprintf "DLESSDASH(%s)" (string_of_hd !wr)
  | CLOBBER -> ">|"
  | If -> "If"
  | Then -> "Then"
  | Else -> "Else"
  | Fi -> "Fi"
  | Elif -> "Elif"
  | LESSGREAT -> "LESSGREAT"
  | LESSAND -> "LESSAND"
  | DGREAT -> "DGREAT"
  | GREATAND -> "GREATAND"
  | WORD w -> Printf.sprintf "WORD(%s)" (unWord w)
  | ASSIGNMENT_WORD w -> 
     Printf.sprintf "ASSIGNMENT_WORD(%s)" (string_of_assignment_word w)
  | NAME w -> Printf.sprintf "NAME(%s)" (unName w)
  | IO_NUMBER io -> Printf.sprintf "IONUMBER(%s)" (string_of_io_number io)
  | Do -> "Do"
  | Done -> "Done"
  | Case -> "Case"
  | Esac -> "Esac"
  | While -> "While"
  | Until -> "Until"
  | For -> "For"
  | Lbrace -> "Lbrace"
  | Rbrace -> "Rbrace"
  | Bang -> "Bang"
  | In -> "In"
  | Pipe -> "Pipe"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | LESS -> "LESS"
  | GREAT -> "GREAT"
  | Uppersand -> "Uppersand"
  | Semicolon -> "Semicolon"
  | NEWLINE -> "Newline"

