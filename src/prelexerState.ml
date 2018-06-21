open CST
open ExtPervasives

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

let push_quoting_mark k b =
  { buffer = QuotingMark k :: b.buffer }

let pop_quotation k b =
  let rec aux squote quote = function
    | [] ->
       (squote, quote, [])
    | QuotingMark k' :: buffer when k = k' ->
       (squote, quote, buffer)
    | (AssignmentMark | QuotingMark _) :: buffer ->
       aux squote quote buffer (* FIXME: Check twice. *)
    | WordComponent (w, WordEmpty) :: buffer ->
       aux (w ^ squote) quote buffer
    | WordComponent (w, c) :: buffer ->
       aux (w ^ squote) (c :: quote) buffer
  in
  (* The last character is removed from the quote since it is the
     closing character. *)
  let buffer = pop_character b.buffer in
  let squote, quote, buffer = aux "" [] buffer in
  let word = Word (squote, quote) in
  let quoted_word =
    match k with
    | SingleQuote -> WordSingleQuoted word
    | DoubleQuote -> WordDoubleQuoted word
  in
  let quote = WordComponent ("\"" ^ squote ^ "\"", quoted_word) in
  { buffer = quote :: buffer }

let push_assignment_mark current =
  { buffer = AssignmentMark :: current.buffer }

let recognize_assignment current =
  let is_assignment_mark = function AssignmentMark -> true | _ -> false in
  let rhs, prefix = take_until is_assignment_mark current.buffer in
  if prefix = current.buffer then (
    current
  ) else
    let current' = { buffer = rhs @ List.tl prefix } in
    match prefix with
    | AssignmentMark :: WordComponent (s, _) :: prefix ->
       assert (s.[String.length s - 1] = '='); (* By after_equal unique call. *)
       (* [s] is a valid name. We have an assignment here. *)
       let lhs = String.(sub s 0 (length s - 1)) in

       (* FIXME: The following check could be done directly with
          ocamllex rules, right?*)

       if Name.is_name lhs then (
         let rhs_string = contents_of_atom_list rhs in
         { buffer =
             WordComponent (s ^ rhs_string,
                            WordAssignmentWord (Name lhs, Word (rhs_string,
                                                                components_of_atom_list rhs)))
             :: prefix
         }
       ) else
         (*
            If [lhs] is not a name, then the corresponding word
            literal must be merged with the preceding one, if it exists.
          *) (
         begin match List.rev rhs with
         | WordComponent (s_rhs, WordLiteral s_rhs') :: rev_rhs ->
            let word = WordComponent (s ^ s_rhs, WordLiteral (s ^ s_rhs')) in
            { buffer = List.rev rev_rhs @ word :: prefix }
         | _ ->
            current'
         end)
    | _ -> current'

(** [(return ?with_newline lexbuf current tokens)] returns a list of
    pretokens consisting of, in that order:

    - WORD(w), where w is the contents of the buffer [current] in case the
      buffer [current] is non-empty;

    - all the elements of [tokens];

    - NEWLINE, in case ?with_newline is true (default: false).

    We know that [tokens] does not contain any Word pretokens. In fact, the
    prelexer produces Word pretokens only from contents he has collected in
    the buffer.

          *)
let return ?(with_newline=false) lexbuf (current : prelexer_state) tokens =
  assert (
      not (List.exists (function (Pretoken.PreWord _)->true |_-> false) tokens)
    );

  let current = recognize_assignment current in

  let flush_word b =
    (* FIXME: Optimise! *)
    let rec aux accu = function
      | WordComponent (s, _) :: b -> aux (s ^ accu) b
      | AssignmentMark :: b -> aux accu b
      | QuotingMark _ :: _ -> assert false
      | [] -> accu
    in
    aux "" b.buffer
  and produce token =
    (* FIXME: Positions are not updated properly. *)
    (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
  in
  let is_digit d =
    Str.(string_match (regexp "^[0-9]+$") d 0)
  in
  let followed_by_redirection = Parser.(function
    | Pretoken.Operator (LESSAND |  GREATAND | DGREAT | CLOBBER |
                         LESS | GREAT | LESSGREAT) :: _ ->
      true
    | _ ->
      false
  ) in

  (*specification

    2.10.1 Shell Grammar Lexical Conventions

    The input language to the shell must be first recognized at the
    character level. The resulting tokens shall be classified by
    their immediate context according to the following rules (applied
    in order). These rules shall be used to determine what a "token"
    is that is subject to parsing at the token level. The rules for
    token recognition in Token Recognition shall apply.

    If the token is an operator, the token identifier for that
    operator shall result.

    If the string consists solely of digits and the delimiter character is
    one of '<' or '>', the token identifier IO_NUMBER shall be
    returned.

    Otherwise, the token identifier TOKEN results.

  *)

  let buffered =
    match flush_word current with
    | "" ->
      []
    | w when is_digit w && followed_by_redirection tokens ->
      [Pretoken.IoNumber w]
    | w ->
      let csts =
        List.(flatten (rev_map (function
            | WordComponent (_, WordEmpty) -> []
            | WordComponent (_, s) -> [s]
            | AssignmentMark -> []
            | QuotingMark _ -> assert false
         ) current.buffer))
      in
      [Pretoken.PreWord (w, csts)]
  in
  let tokens = if with_newline then tokens @ [Pretoken.NEWLINE] else tokens in
  let tokens = buffered @ tokens in
  let out = List.map produce tokens in
  out

let provoke_error current lexbuf =
  return lexbuf current [Pretoken.Operator Parser.INTENDED_ERROR]