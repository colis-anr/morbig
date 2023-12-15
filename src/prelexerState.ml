(**************************************************************************)
(*  Copyright (C) 2017-2023 Yann Régis-Gianas, Nicolas Jeannerod,         *)
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

and quote_kind = SingleQuote | DoubleQuote

module AtomBuffer : sig
  type t
  val get : t -> atom list
  val make : atom list -> t
  val is_empty : t -> bool
  val push_string : t -> string -> t
  val last_line : t -> string
end = struct
  type t = {
    mutable buffer  : atom list;
    mutable strings_len : int;
    mutable strings : string list;
  }

  let too_many_strings = 1024

  let compact_strings strings =
    [String.concat "" (List.rev strings)]

  let compact b =
    if b.strings_len > too_many_strings then (
      b.strings_len <- 1;
      b.strings <- compact_strings b.strings
    );
    b

  let push_string b s =
    match b with
    | WordComponent (s', WordLiteral l) :: csts ->
      let cst = WordComponent (s' ^ s, WordLiteral (l ^ s)) in
      cst :: csts
    | csts ->
      let cst = WordComponent (s, WordLiteral (s)) in
      cst :: csts

  let normalize b =
    if b.strings <> [] then begin
      let s = String.concat "" (List.rev b.strings) in
      let buffer = push_string b.buffer s in
      b.strings <- [];
      b.strings_len <- 0;
      b.buffer <- buffer
    end

  let get b = normalize b; b.buffer

  let make l = { buffer = l; strings_len = 0; strings = [] }

  let is_empty b =
    get b = []

  let push_string b s =
    let b =
      { b with
        strings = s :: b.strings;
        strings_len = b.strings_len + 1
      }
    in
    compact b

  let buffer_as_strings b =
    let rec aux accu = function
      | WordComponent (s, _) :: atoms ->
        aux (s :: accu) atoms
      | _ ->
        accu
    in
    List.rev (aux [] b)

  let last_line b =
    let last_line_of_strings ss =
      let rec aux accu = function
        | s :: ss ->
          if Str.string_match ExtPervasives.newline_regexp s 0 then
            match ExtPervasives.(list_last (lines s)) with
            | None -> assert false (* By the if-condition. *)
            | Some s -> s :: accu
          else
            aux (s :: accu) ss
        | [] ->
          accu
      in
      aux [] ss |> String.concat ""
    in
    if b.strings <> [] then
      last_line_of_strings b.strings
    else
      last_line_of_strings (buffer_as_strings b.buffer)

end

type prelexer_state = {
  nesting_context       : Nesting.t list;
  buffer                : AtomBuffer.t
}

let buffer current =
  AtomBuffer.get (current.buffer)

type t = prelexer_state

let initial_state = {
  nesting_context = [];
  buffer = AtomBuffer.make [];
}

let at_toplevel current =
  match current.nesting_context with
  | [Nesting.HereDocument _] | [] -> true
  | _ -> false

let push_word_component csts w =
  match csts, w with
  | WordComponent (s', WordLiteral l') :: csts, (s, WordLiteral l) ->
    WordComponent (s' ^ s, WordLiteral (l' ^ l)) :: csts
  | _, (s, a) ->
    WordComponent (s, a) :: csts

let push_string b s =
  let buffer = AtomBuffer.push_string b.buffer s in
  { b with buffer }

let parse_pattern : word_component -> word_component list = function
  | WordLiteral w ->
    snd (List.split (PatternMatchingRecognizer.process w))
  | c ->
    [c]

let push_character b c =
  push_string b (String.make 1 c)

let push_separated_string b s =
  let cst = WordComponent (s, WordLiteral s) in
  let buffer = AtomBuffer.make (cst :: buffer b) in
  { b with buffer }

let pop_character = function
  | WordComponent (s, WordLiteral _c) :: buffer ->
    let sequel = try String.(sub s 0 (length s - 1)) with _ -> assert false in
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
  let cst = WordComponent (String.make 1 c, WordEmpty) in
  let buffer = AtomBuffer.make (cst :: buffer b) in
  { b with buffer }

let string_of_word (Word (s, _)) = s

let string_of_attribute = function
  | NoAttribute -> ""
  | ParameterLength -> "#"
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
      (* The ParameterLength attribute is a special case.
         The "#" syntax of the operator shows up _before_ the identifier it modifies. *)
      match attribute with
      | ParameterLength -> "${#" ^ id ^ "}"
      | _ -> "${" ^ id ^ string_of_attribute attribute ^ "}"
    else
      "$" ^ id
  in
  let cst = WordComponent (p, WordVariable v) in
  let buffer = AtomBuffer.make (cst :: buffer b) in
  { b with buffer}

let string_of_atom = function
  | WordComponent (s, _) -> s
  | AssignmentMark -> "|=|"
  | QuotingMark _ -> "|Q|"

let contents_of_atom_list atoms =
  String.concat "" (List.rev_map string_of_atom atoms)

let string_of_atom_list atoms =
  String.concat "#" (List.rev_map string_of_atom atoms)

let contents b =
  contents_of_atom_list (buffer b)

let components_of_atom_list atoms =
  let rec aux accu = function
    | [] -> accu
    | (WordComponent (_, WordEmpty)) :: b -> aux accu b
    | (WordComponent (_, c)) :: b -> aux (c :: accu) b
    | _ :: b -> aux accu b
  in
  aux [] atoms

let components b =
  components_of_atom_list (buffer b)

let push_quoting_mark k b =
  let cst = QuotingMark k in
  let buffer = AtomBuffer.make (cst :: buffer b) in
  { b with buffer }

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
  let squote, quote, buffer = aux "" [] (buffer b) in
  let word = Word (squote, quote) in
  let quoted_word =
    match k with
    | SingleQuote -> WordSingleQuoted word
    | DoubleQuote -> WordDoubleQuoted word
  in
  let squote =
    match k with
    | SingleQuote -> "'" ^ squote ^ "'"
    | DoubleQuote -> "\"" ^ squote ^ "\""
  in
  let quote = WordComponent (squote, quoted_word) in
  let buffer = AtomBuffer.make (quote :: buffer) in
  { b with buffer }

let push_assignment_mark current =
  let buffer = AtomBuffer.make (AssignmentMark :: (buffer current)) in
  { current with buffer }

let is_assignment_mark = function
  | AssignmentMark -> true
  | _ -> false

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
let digit_regexp = Str.regexp "^[0-9]+$"

let return ?(with_newline=false) lexbuf (current : prelexer_state) tokens =
  assert (
    not (List.exists (function (Pretoken.PreWord _)->true |_-> false) tokens)
  );

  let flush_word b =
    let buf = Buffer.create 13 in
    List.iter (function WordComponent (s, _) -> Buffer.add_string buf s
                      | _ -> ()) (List.rev (buffer b));
    Buffer.contents buf
  and produce token =
    (* FIXME: Positions are not updated properly. *)
    (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
  in
  let is_digit d =
    Str.(string_match digit_regexp d 0)
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
          ) (buffer current)))
      in
      (* At this point, we cannot say whether this will be an assignment word,
         so we do a minimal tilde prefixes recognition. *)
      let csts = TildePrefix.recognize ~in_assignment:false csts in
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
let escape_analysis ?(for_backquote=false) ?(for_dquotes=false) level current =
  let current = AtomBuffer.last_line current.buffer in
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
        if for_backquote then [3] else [2]
      | DQuotes :: Backquotes ('`', _) :: _ :: DQuotes :: _ ->
        [2]
      | Backquotes ('`', _) :: DQuotes :: _ ->
        if for_dquotes then [2] else [1]
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
  if Options.debug () then (
    let current' = List.(concat (map rev (map string_to_char_list [current]))) in
    Printf.eprintf "N = %s | %s\n"
      (String.concat " "
         (List.map string_of_int number_of_backslashes_to_escape)
      )
      (string_of_char_list current')
  );

  let backslashes_before = ExtPervasives.count_end_character '\\' current in

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

let escape_analysis_predicate ?(for_backquote=false) ?(for_dquotes=false) level current =
  escape_analysis ~for_backquote ~for_dquotes level current = None

let escaped_double_quote = escape_analysis_predicate ~for_dquotes:true

let escaped_single_quote = escape_analysis_predicate

let escaped_backquote = escape_analysis_predicate ~for_backquote:true

let under_here_document current =
  match current.nesting_context with
  | Nesting.HereDocument (_, _) :: _ -> true
  | _ -> false

let escaped_backquote current =
  escaped_backquote current.nesting_context current

let escaped_single_quote current =
  escaped_single_quote current.nesting_context current

let escaped_double_quote current =
  under_here_document current
  || escaped_double_quote current.nesting_context current

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
  let rec check = function
    | (Nesting.DQuotes | Nesting.HereDocument _) :: _ -> true
    | (Nesting.Backquotes _ | Nesting.Parentheses) :: _ -> false
    | _ :: ss -> check ss
    | _ -> false
  in
  check current.nesting_context

let under_real_double_quote current =
  match current.nesting_context with
  | Nesting.DQuotes :: _ -> true
  | _ -> false

let is_escaping_backslash current _lexbuf c =
  match c with
  | '"' -> escaped_double_quote current
  | '\'' -> escaped_single_quote current
  | '`' -> escaped_backquote current
  | _ -> escape_analysis_predicate current.nesting_context current

let closest_backquote_depth = function
  | [] -> -1
  | Nesting.Backquotes ('`', depth) :: _ -> depth
  | _ -> -1

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
        AtomBuffer.last_line current.buffer
      | Some buffer ->
        Buffer.(
          let n = length buffer in
          let k = String.length delimiter * 2 in
          try sub buffer (max 0 (n - k)) (min k n) with _ -> assert false
        )
    in
    let open QuoteRemoval in
    let preprocess = if dashed then remove_tabs_at_linestart else fun x -> x in
    let last_line = option_map (string_last_line last_chunk) preprocess in
    last_line = Some delimiter
  | _ ->
    false

let remove_contents_suffix pos end_marker contents cst =
  let contents = try
      string_remove_suffix end_marker contents
    with InvalidSuffix _ ->
      (* This situation can happen if the here document is ended by EOF. *)
      raise (Errors.DuringParsing pos)
  in
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
        (string_of_atom_list (buffer current))
  )
