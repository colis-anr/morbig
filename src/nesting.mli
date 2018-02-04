type t =
  | Backquotes of char
  | Parentheses
  | Braces
  | DQuotes

val to_string : t -> string

val of_opening : char -> t

val of_closing : char -> t
