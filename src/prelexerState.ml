(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Nicolas Jeannerod,    *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

open CST
open ExtPervasives

(*specification:

   The shell breaks the input into tokens: words and operators; see
   Token Recognition.

*)
type atom =
  | WordComponent of (string * word_component)
  | QuotingMark of quote_kind
  | AssignmentMark

and quote_kind = SingleQuote | DoubleQuote | OpeningBrace

type lexing_context =
  | Default
  | AssignmentRHS of name

type prelexer_state = {
    lexing_context        : lexing_context;
    nesting_context       : Nesting.t list;
    buffer                : atom list;
}

type t = prelexer_state

let initial_state = {
    lexing_context = Default;
    nesting_context = [];
    buffer = [];
}

let at_toplevel current =
  match current.nesting_context with
  | [Nesting.HereDocument _] | [] -> true
  | _ -> false

let enter_assignment_rhs current name =
  { current with lexing_context = AssignmentRHS name }

let push_word_component csts w =
  match csts, w with
  | WordComponent (s', WordLiteral l') :: csts, (s, WordLiteral l) ->
     WordComponent (s' ^ s, WordLiteral (l' ^ l)) :: csts
  | _, (s, a) ->
     WordComponent (s, a) :: csts

 let push_string b s =
  (* FIXME: Is string concatenation too slow here? *)
  match b.buffer with
  | WordComponent (s', WordLiteral l) :: csts ->
     { b with buffer = WordComponent (s' ^ s, WordLiteral (l ^ s)) :: csts }
  | _ ->
     { b with buffer = WordComponent (s, WordLiteral s) :: b.buffer }

let parse_pattern : word_component -> word_component list = function
  | WordLiteral w ->
     snd (List.split (PatternMatchingRecognizer.process w))
  | c ->
     [c]

let push_character b c =
  push_string b (String.make 1 c)

let push_separated_string b s =
  { b with buffer = WordComponent (s, WordLiteral s) :: b.buffer }

let pop_character = function
  | WordComponent (s, WordLiteral _c) :: buffer ->
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
  { b with buffer = WordComponent (String.make 1 c, WordEmpty) :: b.buffer }

let string_of_word (Word (s, _)) = s

let string_of_attribute = function
  | NoAttribute -> ""
  | ParameterLength w -> string_of_word w
  | UseDefaultValues (p, w) -> p ^ string_of_word w
  | AssignDefaultValues (p, w) -> p ^ string_of_word w
  | IndicateErrorifNullorUnset (p, w) -> p ^ string_of_word w
  | UseAlternativeValue (p, w) -> p ^ string_of_word w
  | RemoveSmallestSuffixPattern w -> "%" ^ string_of_word w
  | RemoveLargestSuffixPattern w -> "%%" ^ string_of_word w
  | RemoveSmallestPrefixPattern w -> "#" ^ string_of_word w
  | RemoveLargestPrefixPattern w -> "##" ^ string_of_word w

let push_parameter ?(with_braces=false) ?(attribute=NoAttribute) b id =
  let v = VariableAtom (id, attribute) in
  let p =
    if with_braces then
      "${" ^ id ^ string_of_attribute attribute ^ "}"
    else
      "$" ^ id
  in
  { b with buffer = WordComponent (p, WordVariable v) :: b.buffer }

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
  { b with buffer = QuotingMark k :: b.buffer }

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
(*  let buffer = pop_character b.buffer in *)
  let squote, quote, buffer = aux "" [] b.buffer in
  let word = Word (squote, quote) in
  let quoted_word =
    match k with
    | SingleQuote -> WordSingleQuoted word
    | DoubleQuote -> WordDoubleQuoted word
    | OpeningBrace -> WordDoubleQuoted word
  in
  let squote =
    match k with
    | SingleQuote -> "'" ^ squote ^ "'"
    | DoubleQuote -> "\"" ^ squote ^ "\""
    | OpeningBrace -> squote
  in
  let quote = WordComponent (squote, quoted_word) in
  { b with buffer = quote :: buffer }

let push_assignment_mark current =
  { current with buffer = AssignmentMark :: current.buffer }

let is_assignment_mark = function
  | AssignmentMark -> true
  | _ -> false

let recognize_assignment current =
  let rhs, prefix = take_until is_assignment_mark current.buffer in
  if prefix = current.buffer then (
    current
  ) else
    let current' = { current with buffer = rhs @ List.tl prefix } in
    match prefix with
    | AssignmentMark :: WordComponent (s, _) :: prefix ->
       assert (s.[String.length s - 1] = '='); (* By after_equal unique call. *)
       (* [s] is a valid name. We have an assignment here. *)
       let lhs = String.(sub s 0 (length s - 1)) in

       (* FIXME: The following check could be done directly with
          ocamllex rules, right?*)

       if Name.is_name lhs then (
         let rhs_string = contents_of_atom_list rhs in
         { current with buffer =
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
            { current with buffer = List.rev rev_rhs @ word :: prefix }
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
      | QuotingMark _ :: b -> aux accu b
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
    | Pretoken.Operator (LESSAND |  GREATAND | DGREAT | DLESS _
                         | CLOBBER | LESS | GREAT | LESSGREAT) :: _ ->
      true
    | _ ->
      false
  ) in

  (*specification:

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
            | QuotingMark _ -> []
         ) current.buffer))
      in
      [Pretoken.PreWord (w, csts)]
  in
  let tokens = if with_newline then tokens @ [Pretoken.NEWLINE] else tokens in
  let tokens = buffered @ tokens in
  List.map produce tokens

exception NotAWord of string

let word_of b =
  let rec aux w cst = Pretoken.(function
      | [] ->
         Word (w, cst)
      | (p, _, _) :: ps ->
         match preword_of_pretoken p with
         | EOF -> assert false (* Because of [word_of] calling context. *)
         | PreWord (w', cst') -> aux (w ^ w') (cst @ cst') ps
         | _ -> assert false (* By preword_of_pretoken. *)
  ) in
  aux "" [] b

let located_word_of = function
  | ((Pretoken.PreWord (w, cst), p1, p2)) :: _ -> (Word (w, cst), p1, p2)
  | (p, _, _) :: _ -> raise (NotAWord (Pretoken.string_of_pretoken p))
  | [] -> raise (NotAWord "empty")

let provoke_error current lexbuf =
  return lexbuf current [Pretoken.Operator Parser.INTENDED_ERROR]

(**
   A double quote can be escaped if we are already inside (at least)
   two levels of quotation. For instance, if the input is <dquote>
   <dquote> <backslash><backslash> <dquote> <dquote> <dquote>, the
   escaped backslash is used to escape the quote character.

*)
let escape_analysis ?(for_backquote=false) level current =
  let current =
    List.map
      (function
       | WordComponent (s, _) -> s
       | _ -> "")
      current.buffer
  in
  let number_of_backslashes_to_escape = Nesting.(
    (* FIXME: We will be looking for the general pattern here. *)
    match level with
    | Backquotes ('`', _) :: Backquotes ('`', _) :: Backquotes ('`', _) :: _ ->
       [3]
    | Backquotes ('`', _) :: Backquotes ('`', _) :: _ ->
       [2]
    | DQuotes :: Backquotes ('`', _) :: [] ->
       [1; 2]
    | DQuotes :: Backquotes ('`', _) :: DQuotes :: _ ->
       [2]
    | DQuotes :: Backquotes ('`', _) :: _ :: DQuotes :: _ ->
       [2]
    | Backquotes ('`', _) :: DQuotes :: _ ->
       [2]
    | Backquotes ('`', _) :: _ :: DQuotes :: _ ->
       [2]
    | [Backquotes ('`', _)] ->
       if for_backquote then
         [3]
       else
         [1; 2]
    | _ ->
       [1]
  )
  in
  let current' = List.(concat (map rev (map string_to_char_list current))) in

  if Options.debug () then
    Printf.eprintf "N = %s | %s\n"
      (String.concat " "
         (List.map string_of_int number_of_backslashes_to_escape)
      )
      (string_of_char_list current');

  let backslashes_before = preceding '\\' current' in

  if List.exists (fun k ->
         backslashes_before >= k && (k - backslashes_before) mod (k + 1) = 0
     ) number_of_backslashes_to_escape
  then (
    (** There is no special meaning for this character. It is
        escaped. *)
    None
  ) else
    (**
        The character preceded by this sequence is not escaped.
        In the case of `, the interpretation of this character
        depends on the number of backslashes the precedes it.
        Typically, in:

        echo `echo \`foo\``

        The second <backquote> is not escaped BUT it is not
        closing the current subshell, it is opening a new
        one.

     *)
    Some backslashes_before

let escape_analysis_predicate ?(for_backquote=false) level current =
  escape_analysis ~for_backquote level current = None

let escaped_double_quote = escape_analysis_predicate

let escaped_single_quote = escape_analysis_predicate

let escaped_backquote = escape_analysis_predicate ~for_backquote:true

let escaped_backquote current =
  escaped_backquote current.nesting_context current

let escaped_single_quote current =
  escaped_single_quote current.nesting_context current

let escaped_double_quote current =
  escaped_double_quote current.nesting_context current

let nesting_context current =
  current.nesting_context

let enter what current =
  let nesting_context = what :: current.nesting_context in
  { current with nesting_context }

let enter_double_quote =
  enter Nesting.DQuotes

let enter_here_document dashed delimiter =
  enter (Nesting.HereDocument (dashed, delimiter))

let enter_braces =
  enter Nesting.Braces

let quit_double_quote current =
  match current.nesting_context with
  | Nesting.DQuotes :: nesting_context -> { current with nesting_context }
  | _ -> assert false

let quit_braces current =
  match current.nesting_context with
  | Nesting.Braces :: nesting_context -> { current with nesting_context }
  | _ -> assert false

let enter_backquotes op escaping_level current =
  let nesting_context =
    Nesting.Backquotes (op, escaping_level) :: current.nesting_context
  in
  { current with nesting_context }

let under_backquote current =
  match list_hd_opt current.nesting_context with
  | Some (Nesting.Backquotes ('`', _)) -> true
  | _ -> false

let under_braces current =
  match list_hd_opt current.nesting_context with
  | Some Nesting.Braces -> true
  | _ -> false

let under_backquoted_style_command_substitution current =
  Nesting.under_backquoted_style_command_substitution current.nesting_context

let under_double_quote current =
  match current.nesting_context with
  | (Nesting.DQuotes | Nesting.HereDocument _) :: _ -> true
  | _ -> false

let under_real_double_quote current =
  match current.nesting_context with
  | Nesting.DQuotes :: _ -> true
  | _ -> false

let under_here_document current =
  match current.nesting_context with
  | Nesting.HereDocument _ :: _ -> true
  | _ -> false

let is_escaping_backslash current _lexbuf c =
  match c with
  | '"' -> escaped_double_quote current
  | '\'' -> escaped_single_quote current
  | '`' -> escaped_backquote current
  | _ -> escape_analysis_predicate current.nesting_context current

let rec closest_backquote_depth = function
  | [] -> -1
  | Nesting.Backquotes ('`', depth) :: _ -> depth
  | _ :: nesting -> closest_backquote_depth nesting

let backquote_depth current =
  let current_depth =
    escape_analysis ~for_backquote:true current.nesting_context current
    |> function
      | Some d -> d
      | None -> assert false (* By usage of backquote_depth. *)
  in
  if Options.debug () then
    Printf.eprintf "Backquote depth: %d =?= %d\n"
      current_depth
      (closest_backquote_depth current.nesting_context);
  if current_depth = closest_backquote_depth current.nesting_context then
    None
  else
    Some current_depth

let found_current_here_document_delimiter ?buffer current =
  match current.nesting_context with
  | Nesting.HereDocument (dashed, delimiter) :: _ ->
     let last_chunk =
       match buffer with
         | None ->
            contents current
         | Some buffer ->
            Buffer.(
             let n = length buffer in
             let k = String.length delimiter * 2 in
             sub buffer (max 0 (n - k)) (min k n)
            )
     in
     let open QuoteRemoval in
     let preprocess = if dashed then remove_tabs_at_linestart else fun x -> x in
     let last_line = option_map (string_last_line last_chunk) preprocess in
     last_line = Some delimiter
  | _ ->
     false

let remove_contents_suffix end_marker (contents : string) (cst : CST.word_cst) =
  let contents = string_remove_suffix end_marker contents in
  let rec aux cst =
    match cst with
    | (WordLiteral contents) :: cst ->
       begin match lines contents with
       | [] | [_] ->
          aux cst
       | rest ->
          let rest = List.(rev (tl (rev rest))) in
          let suffix = String.concat "\n" rest ^ "\n" in
          WordLiteral suffix :: cst
       end
    | _ :: cst ->
       aux cst
    | [] ->
       []
  in
  contents, List.(rev (aux (rev cst)))

let debug ?(rule="") lexbuf current = Lexing.(
    if Options.debug () then
      let curr_pos =
        min lexbuf.lex_curr_pos lexbuf.lex_buffer_len
      in
      Printf.eprintf "\
                      %s [ ] %s     { %s } %s @ %s #\n[%s]\n"
        (Bytes.(to_string (sub lexbuf.lex_buffer 0 curr_pos)))
        (let k = lexbuf.lex_buffer_len - curr_pos - 1 in
         if k > 0 then
           Bytes.(to_string (sub lexbuf.lex_buffer curr_pos k))
         else "")
        (Lexing.lexeme lexbuf)
        rule
        (String.concat " " (List.map Nesting.to_string current.nesting_context))
        (string_of_atom_list current.buffer)
)
