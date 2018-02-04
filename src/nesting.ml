type t =
  | Backquotes of char
  | Parentheses
  | Braces
  | DQuotes

let to_string = function
  | Backquotes c -> Printf.sprintf "@%c" c
  | Parentheses -> "("
  | Braces -> "{"
  | DQuotes -> "\""

let of_opening c =
  if c = '(' then Parentheses
  else if c = '{' then Braces
  else if c = '`' then Backquotes c
  else failwith "Unrecognized nesting."

let of_closing c =
  if c = ')' then Parentheses
  else if c = '}' then Braces
  else if c = '`' then Backquotes c
  else failwith "Unrecognized nesting."
