open Position

type word = Word of string
[@@deriving yojson]

type name = Name of string
[@@deriving yojson]

type 'a assignment_word = AssignmentWord of name * 'a
[@@deriving yojson]

type io_number = IONumber of string
[@@deriving yojson]

let unWord (Word s) = s

let unName (Name s) = s

let word_of_name (Name w) = Word w

let word_of_assignment_word (AssignmentWord (Name n, s)) =
  Word (n ^ "=" ^ s)

let string_of_word (Word s) = s

module NameSet = Set.Make (struct
  type t = name
  let compare (Name s1) (Name s2) = String.compare s1 s2
end)

let pp_word ppf = function
  | Word s ->
     Format.fprintf ppf "%s" s

let pp_name ppf = function
  | Name s ->
     Format.fprintf ppf "%s" s

let pp_assignment_word ppf a =
  let AssignmentWord (n, w) = a in
  Format.fprintf ppf "%a=%a" pp_name n pp_word w

let pp_io_number ppf (IONumber s) =
  Format.fprintf ppf "IONumber %s" s
