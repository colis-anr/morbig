open CST

(**specification:

   The shell breaks the input into tokens: words and operators; see
   Token Recognition.

*)
type atom =
  | WordComponent of (string * word_component)
  | QuotingMark of quote_kind
  | AssignmentMark
and quote_kind = SingleQuote | DoubleQuote

type prelexer_state = {
    buffer                : atom list;
}

let initial_state = {
    buffer = [];
}

let push_string b s =
  (* FIXME: Is string concatenation too slow here? *)
  match b.buffer with
  | WordComponent (s', WordLiteral l) :: csts ->
     { buffer = WordComponent (s' ^ s, WordLiteral (l ^ s)) :: csts }
  | _ ->
     { buffer = WordComponent (s, WordLiteral s) :: b.buffer }

let push_character b c =
  push_string b (String.make 1 c)

let push_separated_string b s =
  { buffer = WordComponent (s, WordLiteral s) :: b.buffer }

let pop_character = function
  | WordComponent (s, WordLiteral c) :: buffer ->
     let sequel = String.(sub s 0 (length s - 1)) in
     if sequel = "" then
       buffer
     else
       WordComponent (sequel, WordLiteral sequel) :: buffer
  | _ ->
     assert false

(** [push_word_closing_character b c] push a character [c] to mark it
    as part of the string representing the current word literal but
    with no interpretation as a word CSTs. Typically, if the word
    is "$(1)", the string representing the current word is "$(1)"
    so the character ')' must be pushed as part of this string
    representation but ')' is already taken care of in the word
    CST [WordSubshell (_, _)] associated to this word so we do not
    push ')' as a WordLiteral CST. *)
let push_word_closing_character b c =
  { buffer = WordComponent (String.make 1 c, WordEmpty) :: b.buffer }

let string_of_atom = function
  | WordComponent (s, _) -> s
  | AssignmentMark -> "|=|"
  | QuotingMark _ -> "|Q|"

let contents_of_atom_list atoms =
  String.concat "" (List.rev_map string_of_atom atoms)

let string_of_atom_list atoms =
  String.concat "#" (List.rev_map string_of_atom atoms)

let contents b =
  contents_of_atom_list b.buffer

let components_of_atom_list atoms =
  let rec aux accu = function
    | [] -> accu
    | (WordComponent (_, WordEmpty)) :: b -> aux accu b
    | (WordComponent (_, c)) :: b -> aux (c :: accu) b
    | _ :: b -> aux accu b
  in
  aux [] atoms

let components b =
  components_of_atom_list b.buffer

