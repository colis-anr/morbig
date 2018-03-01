(** -*- tuareg -*- ********************************************************)
(*                                                                        *)
(*  Copyright (C) 2017,2018 Yann Régis-Gianas, Nicolas Jeannerod,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

(**

   This module implements the token recognizer when it is not in the mode
   that recognizes here-documents as specified by:

              http://pubs.opengroup.org/onlinepubs/9699919799/
              2.3 Token Recognition

*)

{

(**specification:

   The shell breaks the input into tokens: words and operators; see
   Token Recognition.

*)
open Lexing
open ExtPervasives
open Parser

(* FIXME: The name "Word" is confusing here. It must be changed. *)
type pretoken =
  | Word of string
  | IoNumber of string
  | Operator of Parser.token
  | EOF
  | NEWLINE

let string_of_pretoken = function
  | Word s -> Printf.sprintf "WORD(%s)" s
  | IoNumber s -> Printf.sprintf "IONUM(%s)" s
  | Operator t -> Printf.sprintf "OPERATOR(%s)" (Token.string_of_token t)
  | EOF -> "EOF"
  | NEWLINE -> "NEWLINE"

let push_character b c =
  String.make 1 c :: b

let contents b =
  String.concat "" (List.rev b)

let string_last_char s =
  String.(s.[length s - 1])

let string_minus_last_char s =
  String.(sub s 0 (length s - 1))

(* FIXME: Probably incorrect: Must split buffer into character first. *)
let rec preceded_by n c cs =
  n = 0 || match cs with
           | [] -> n = 0
           | c' :: cs -> c' = c && preceded_by (n - 1) c cs

let push_string b s =
  s :: b

(** [(return ?with_newline lexbuf current tokens)] returns a list of
    pretokens consisting of, in that order:

    - WORD(w), where w is the contents of the buffer [current] in case the
      buffer [current] is non-empty;

    - all the elements of [tokens];

    - NEWLINE, in case ?with_newline is true (default: false).

    We know that [tokens] does not contain any Word pretokens. In fact, the
    prelexer produces Word pretokens only from contents he has collected in
    the buffer.

    Side effect: the buffer [current] is reset to empty.
 *)
let return ?(with_newline=false) lexbuf current tokens =
  assert (not (List.exists (function (Word _)->true|_->false) tokens));
  let flush_word b =
    contents b
  and produce token =
    (* FIXME: Positions are not updated properly. *)
    (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
  in
  let is_digit d =
    Str.(string_match (regexp "^[0-9]+$") d 0)
  in
  let followed_by_redirection = function
    | Operator (LESSAND |  GREATAND | DGREAT | CLOBBER |
                LESS | GREAT | LESSGREAT) :: _ ->
      true
    | _ ->
      false
  in

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
      [IoNumber w]
    | w ->
      [Word w]
  in
  let tokens = if with_newline then tokens @ [NEWLINE] else tokens in
  let tokens = buffered @ tokens in
  List.map produce tokens

let provoke_error current lexbuf =
  return lexbuf current [Operator INTENDED_ERROR]

let operators = Hashtbl.create 17     ;;
Hashtbl.add operators "&&"  AND_IF    ;;
Hashtbl.add operators "||"  OR_IF     ;;
Hashtbl.add operators ";;"  DSEMI     ;;
Hashtbl.add operators "<&"  LESSAND   ;;
Hashtbl.add operators ">&"  GREATAND  ;;
Hashtbl.add operators "<>"  LESSGREAT ;;
Hashtbl.add operators ">>"  DGREAT    ;;
Hashtbl.add operators ">|"  CLOBBER   ;;
Hashtbl.add operators "|"   Pipe      ;;
Hashtbl.add operators "("   Lparen    ;;
Hashtbl.add operators ")"   Rparen    ;;
Hashtbl.add operators "<"   LESS      ;;
Hashtbl.add operators ">"   GREAT     ;;
Hashtbl.add operators ";"   Semicolon ;;
Hashtbl.add operators "&"   Uppersand ;;

let optoken_of_string s =
  try
    Operator (Hashtbl.find operators s)
  with Not_found ->
    Printf.eprintf
      "Internal error: `%s' is not a valid operator token.\n"
       s;
    assert false

let string_of_level l = String.concat " : " (List.map Nesting.to_string l)

let under_double_quotes level =
  match level with
  | Nesting.DQuotes :: _ -> true
  | _ -> false
(*  List.mem Nesting.DQuotes level *)


let rec under_backquoted_style_command_substitution = function
  | [] -> false
  | Nesting.Backquotes '`' :: _ -> true
  | Nesting.Backquotes '(' :: _ -> false
  | _ :: level -> under_backquoted_style_command_substitution level

(** A double quote can be escaped if we are already inside (at least)
   two levels of quotation. For instance, if the input is <dquote>
   <dquote> <backslash><backslash> <dquote> <dquote> <dquote>, the
   escaped backslash is used to escape the quote character.

   More generally, if [level] contains n nested double quotes,
   2^{n - 2} backslashes are necessary to escape a double quote.

   FIXME: Check this statement.
*)
let escaped_double_quote level current =
  let number_of_backslashes_to_escape = Nesting.(
    match level with
    | DQuotes :: Backquotes '`' :: DQuotes :: _ -> 2
    | DQuotes :: Backquotes '`' :: _ :: DQuotes :: _ -> 2
    | DQuotes :: Backquotes '`' :: _ -> 1
    | Backquotes '`' :: DQuotes :: _ -> 2
    | Backquotes '`' :: _ :: DQuotes :: _ -> 2
    | _ -> 1
  ) in
  let escape_sequence =
    repeat number_of_backslashes_to_escape (fun _ -> '\\')
  in

  let remove_escaped_backslashes current =
    let rec trim = function
      | [] ->
         []
      | '\\' :: cs ->
         let cs' = trim cs in
         if fst (take number_of_backslashes_to_escape cs')
            = escape_sequence
         then
           snd (take number_of_backslashes_to_escape cs')
         else
           '\\' :: cs'
      | c :: cs ->
         c :: trim cs
    in
    trim current
  in
  let current' = List.(concat (map rev (map string_to_char_list current))) in
  let current' =
    (* FIXME: Justify this! *)
    if not (under_backquoted_style_command_substitution level) then
      remove_escaped_backslashes current'
    else
      current'
  in
  preceded_by number_of_backslashes_to_escape '\\' current'

let escaped_single_quote = escaped_double_quote

let here_document_placeholder () =
  ref (CST.({
      value = Word "<you should not see this>";
      position = CSTHelpers.dummy_position
  }))

let subshell_parsing op level lexbuf =
    let copy_position p =
      Lexing.{ p with pos_fname = p.pos_fname }
    in

    let lexbuf' =
      Lexing.{ lexbuf with
          lex_buffer = Bytes.copy lexbuf.lex_buffer;
          lex_mem = Array.copy lexbuf.lex_mem;
          lex_start_p = copy_position lexbuf.lex_start_p;
          lex_curr_p = copy_position lexbuf.lex_curr_p;
      }
    in
    let level = Nesting.Backquotes op :: level in
    ignore ((!RecursiveParser.parse) level lexbuf');
    lexbuf'.Lexing.lex_curr_p.Lexing.pos_cnum
    - lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum

exception LexingError of string

let lexing_error msg =
  raise (LexingError msg)

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let digit = ['0'-'9']

let operator = "&&" | "||" | ";;" |
               "<<" | ">>" | "<&" | ">&" | "<>" | "<<-" |
               ">|" |
               "|" | "(" | ")" | "<" | ">" | ";" | "&"



(**specification:

   When it is not processing an io_here, the shell shall break its
   input into tokens by applying the first applicable rule below to the
   next character in its input. The token shall be from the current
   position in the input until a token is delimited according to one of
   the rules below; the characters forming the token are exactly those in
   the input, including any quoting characters. If it is indicated that a
   token is delimited, and no characters have been included in a token,
   processing shall continue until an actual token is delimited.

*)

(** The previous paragraph extracted from the specification implies
    that a tool like lex can be used to implement the token
    recognition pass. *)

rule token level current = parse

(**specification:

   If the end of input is recognized, the current token shall be
   delimited. If there is no current token, the end-of-input indicator
   shall be returned as the token.

*)
  | eof {
    if level = [] then
      return lexbuf current [EOF]
    else (
      lexing_error "Unterminated nesting!"
    )
  }

(** Quotations *)

(**specification:

   If the current character is <backslash>, single-quote, or
   double-quote and it is not quoted, it shall affect quoting for
   subsequent characters up to the end of the quoted text. The rules for
   quoting are as described in Quoting . During token recognition no
   substitutions shall be actually performed, and the result token shall
   contain exactly the characters that appear in the input (except for
   <newline> joining), unmodified, including any embedded or enclosing
   quotes or substitution operators, between the <quotation-mark> and the
   end of the quoted text. The token shall not be delimited by the end of
   the quoted field.

*)

(**specification

   2.2.1 Escape Character (Backslash)

   A <backslash> that is not quoted shall preserve the literal value of
   the following character, with the exception of a <newline>. If a
   <newline> follows the <backslash>, the shell shall interpret this as
   line continuation. The <backslash> and <newline> shall be removed
   before splitting the input into tokens. Since the escaped <newline> is
   removed entirely from the input and is not replaced by any white
   space, it cannot serve as a token separator.

*)
  | '\\' newline {
    token level current lexbuf
  }

  | '\\' (_ as c) {
    let current =
      if under_backquoted_style_command_substitution level then
          match c with
            | '$' | '\\' | '`' ->
              push_character current c
            | '"' ->
               let current = push_character current '\\' in
               if escaped_double_quote level current then
                 push_character current c
               else
                 double_quotes (Nesting.DQuotes :: level) current lexbuf
            | c ->
              push_string current (Lexing.lexeme lexbuf)
      else
        push_string current (Lexing.lexeme lexbuf)
    in
    token level current lexbuf
  }

(**specification

   2.2.2 Single-Quotes

*)
  | '\'' {
    let current' = push_character current '\'' in
    if (* under_double_quotes level || *) escaped_single_quote level current then (
      token level (push_character current '\'') lexbuf
    ) else
      let current = single_quotes current' lexbuf in
      token level current lexbuf
  }


(** specification

    2.2.3 Double-Quotes

*)
  | '"' {
    let is_escaped = escaped_double_quote level current in
    let current = push_character current '"' in
    let current =
      if not is_escaped then
        double_quotes (Nesting.DQuotes :: level) current lexbuf
      else
        current
    in
    token level current lexbuf
  }

(**

   The following three rules of the specification are implemented
   by the longest match/first rule strategy of lex.

*)
(* FIXME: To be checked. *)

(**specification:

   If the previous character was used as part of an operator and the
   current character is not quoted and can be used with the current
   characters to form an operator, it shall be used as part of that
   (operator) token.

*)

(**specification:

   If the previous character was used as part of an operator and the
   current character cannot be used with the current characters to form
   an operator, the operator containing the previous character shall be
   delimited.

*)

(**specification:

  If the current character is not quoted and can be used as the first
  character of a new operator, the current token (if any) shall be
  delimited. The current character shall be used as the beginning of the
  next (operator) token.

*)

(**specification:

   /* The following are the operators mentioned above. */


   %token  AND_IF    OR_IF    DSEMI
   /*      '&&'      '||'     ';;'    */


   %token  DLESS  DGREAT  LESSAND  GREATAND  LESSGREAT  DLESSDASH
   /*      '<<'   '>>'    '<&'     '>&'      '<>'       '<<-'   */


   %token  CLOBBER
   /*      '>|'   */

*)
  | "<<" {
    return lexbuf current [Operator (DLESS (here_document_placeholder ()))]
  }
  | "<<-" {
    return lexbuf current [Operator (DLESSDASH (here_document_placeholder ()))]
  }

(**specification

   If the current character is an unquoted '$' or '`', the shell shall
   identify the start of any candidates for parameter expansion
   (Parameter Expansion), command substitution (Command Substitution),
   or ] arithmetic expansion (Arithmetic Expansion) from their
   introductory unquoted character sequences: '$' or "${", "$(" or
   '`', and "$((", respectively. The shell shall read sufficient input
   to determine the end of the unit to be expanded (as explained in
   the cited sections). While processing the characters, if instances
   of expansions or quoting are found nested within the substitution,
   the shell shall recursively process them in the manner specified
   for the construct that is found. The characters found from the
   beginning of the substitution to its end, allowing for any
   recursion necessary to recognize embedded constructs, shall be
   included unmodified in the result token, including any embedded or
   enclosing substitution operators or quotes. The token shall not be
   delimited by the end of the substitution.

*)

  (* $# is a special parameter, that is a # after an $ does not start a
     comment *)
  | ("$" "#"?) as s {
    token level (push_string current s) lexbuf
  }

  (* FIXME: Handle nesting *)
  | '`' as op | "$" ('(' as op) {
    if op = '`' && list_hd_opt level = Some (Nesting.Backquotes op) then begin
        let pos = lexbuf.lex_curr_p.pos_cnum in
        lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = pos - 1 };
    (*

        If the last nesting symbol of [level] is a Backquote, the
        current backquote is the closing symbol.

    *)
        return lexbuf current [EOF]
    end else
    (*

        Otherwise, we have to invoke a recursive parser to parse a prefix
        of the remaining input.

    *)
      let current = push_string current (Lexing.lexeme lexbuf) in
      let current = subshell op level current lexbuf in
      let current = close_subshell current lexbuf in
      token level current lexbuf
  }

  | ")" {
      return lexbuf current [Operator Rparen]
  }

  | "(" {
      return lexbuf current [Operator Lparen]
  }

  | operator as s {
    let operator = optoken_of_string s in
    return lexbuf current [operator]
  }

| "$((" {
    let current = push_string current "$((" in
    let current = next_double_rparen level 1 current lexbuf in
    token level current lexbuf
  }

(**specification

   Within the string of characters from an enclosed "${" to the
   matching '}', an even number of unescaped double-quotes or
   single-quotes, if any, shall occur. A preceding <backslash>
   character shall be used to escape a literal '{' or '}'. The rule in
   Parameter Expansion shall be used to determine the matching '}'.

*)
  | "${" {
    let current = push_string current "${" in
    let current = next_nesting (Nesting.Braces :: level) current lexbuf in
    token level current lexbuf
  }

(**specification:

  If the current character is an unquoted <newline>, the current
  token shall be delimited.

*)
  | newline {
    return ~with_newline:true lexbuf current []
  }

(**specification:

  If the current character is an unquoted <blank>, any token
  containing the previous character is delimited and the current
  character shall be discarded.

*)
  | blank {
    return lexbuf current []
  }

(**specification:

  If the current character is a '#', it and all subsequent characters
  up to, but excluding, the next <newline> shall be discarded as a
  comment. The <newline> that ends the line is not considered part of
  the comment.

*)
(**

    As <newline> is a token delimiter, we can flush the current
    word.

*)
  | '#' as c
  {
    (**

       There two cases depending on the characters on the left of '#':
       If '#' is preceded by a separator, it is starting a comment.
       otherwise, '#' is part of a word.

    *)
    if current = [] then
      comment lexbuf
    else
      token level (push_character current c) lexbuf
  }

(**specification

   [Assignment preceding command name]

   [When the first word]

   If the TOKEN does not contain the character '=', rule 1 is
   applied. Otherwise, 7b shall be applied.

   [Not the first word]

   If the TOKEN contains the <equals-sign> character:

   If it begins with '=', the token WORD shall be returned.

   If all the characters preceding '=' form a valid name (see XBD
   Name), the token ASSIGNMENT_WORD shall be returned. (Quoted
   characters cannot participate in forming a valid name.)

   Otherwise, it is unspecified whether it is ASSIGNMENT_WORD or WORD
   that is returned.
*)
  | '=' as c {
    (** Most of the work is done in the lexer.*)
    let current = push_character current c in
    after_equal level current lexbuf
  }

    (* FIXME: next comment incomprehensible *)
(**

   If the previous rules are character, the semantic of lex
   specification ensures that the remaining rule implements
   the following two rules of the specification:

 *)

(**specification:

   If the previous character was part of a word, the current character
   shall be appended to that word.

*)

(**specification:

  The current character is used as the start of a new word.

*)
    (* FIXME: can we really accept *anything* here ? *)
  | _ as c {
    token level (push_character current c) lexbuf
  }

and skip k current = parse
| _ as c {
  let current = push_character current c in
  if k <= 1 then current else skip (k - 1) current lexbuf
}

and close op = parse
| "`" {
  if op <> '`' then lexing_error "Lexing error: unbalanced backquotes."
}
| (")" | "}" as op') {
  if (op = '(' && op' <> ')') || (op = '{' && op' <> '}') then
    lexing_error (Printf.sprintf "Lexing error: unbalanced $%c." op)
}
| _ as c {
  lexing_error (Printf.sprintf "Unclosed %c (got %c)." op c)
}

and comment = parse
| [^'\n''\r']* newline {
    Lexing.new_line lexbuf;
    return ~with_newline:true lexbuf [] []
  }
| '#' [^'\n''\r']* eof {
    return ~with_newline:false lexbuf [] []
  }


and subshell op level current = parse
  | "" {
    let consumed = subshell_parsing op level lexbuf in
    if consumed > 0 then skip (consumed) current lexbuf else current
  }

and close_subshell current = parse
  | (")" | "`") as c {
     push_character current c
  }
  | (blank | newline) as c {
     let current = push_string current c in
     close_subshell current lexbuf
  }
  | ("\\" newline) as c {
     let current = push_string current c in
     close_subshell current lexbuf
  }
  | eof {
     lexing_error (Printf.sprintf "Unclosed subshell (got EOF).")
  }
  | _ as c {
     lexing_error (Printf.sprintf "Unclosed subshell (got '%c')." c)
  }

and return_subshell op level current = parse
 | "" {
    let current = subshell op level current lexbuf in
    let current = close_subshell current lexbuf in
    return lexbuf current []
 }

and after_equal level current = parse
  | '`' as op | "$" ( '(' as op) {
    if op = '`' && list_hd_opt level = Some (Nesting.Backquotes op) then
     (*

         If the last nesting symbol of [level] is a Backquote, the
         current backquote is the closing symbol.

     *)
      provoke_error current lexbuf
    else
      let current =
        subshell op level (push_string current (Lexing.lexeme lexbuf)) lexbuf
      in
      let current =
        close_subshell current lexbuf
      in
      after_equal level current lexbuf
  }

  | "$((" {
    let current = push_string current "$((" in
    let current = next_double_rparen level 1 current lexbuf in
    token level current lexbuf
  }

  | "(" | "{" as op {
    let current = push_character current op in
    after_equal (Nesting.of_opening op :: level) current lexbuf
  }

  | ")" | "}" as op {
    begin match level with
      | Nesting.Backquotes '(' :: level when op = ')' ->
         provoke_error current lexbuf
      | Nesting.Backquotes '`' :: level when op = '`' ->
         provoke_error current lexbuf
      | nestop :: level when nestop = Nesting.of_closing op ->
         let current = push_character current op in
         after_equal level current lexbuf
      | _ ->
         (* FIXME *)
         let x =
           match level with
           | [] -> "X"
           | n :: level -> Nesting.to_string n
         in
         lexing_error (Printf.sprintf
                     "Lexing error: unbalanced parentheses (%c <> %s)"
                   op x)
    end
  }
  | "\"" {
    let current = push_character current '"' in
    let current = double_quotes (Nesting.DQuotes :: level) current lexbuf in
    after_equal level current lexbuf
  }
  | "\'" {
    let current = push_character current '\'' in
    let current = single_quotes current lexbuf in
    after_equal level current lexbuf
  }
  | '\\' _ {
    let current = push_string current (Lexing.lexeme lexbuf) in
    after_equal level current lexbuf
  }
  (* FIXME: Factorize the following two rules. *)
  | newline {
    let result =
      (* FIXME: See next fixme, this is not the right condition. *)
      if not (under_double_quotes level) then
        return ~with_newline:true lexbuf current []
      else (
        let current = push_character current ' ' in
        after_equal level current lexbuf
      )
    in
    Lexing.new_line lexbuf;
    result
  }
  | blank {
    (* FIXME: Probably not the right condition to consider this blank as a
       separator given the current nesting context. *)
    let is_blank_separator =
      not (
          under_double_quotes level
          || (match level with
              | (Nesting.Braces | Nesting.Parentheses) :: _ -> true
              | _ -> false)
        )
    in
    if is_blank_separator then (
      return ~with_newline:false lexbuf current []
    ) else (
      let current = push_character current ' ' in
      after_equal level current lexbuf
    )
  }
  (* FIXME: which other operators shall be accepted as delimiters here ?*)
  | (";;" | ";") as s {
    if level = [] then
      return lexbuf current [optoken_of_string s]
    else
      let current = push_string current s in
      after_equal level current lexbuf
  }
  | _ as c {
    let current = push_character current c in
    after_equal level current lexbuf
  }
  | eof {
    return lexbuf current []
  }

and next_nesting level current = parse
  | '`' as op {
      match level with
      | Nesting.Backquotes _ :: _ ->
        push_character current op
      | _ ->
        let current = push_character current op in
        next_nesting level current lexbuf
  }
  | "{" | "(" as op {
    let current = push_character current op in
    next_nesting (Nesting.of_opening op :: level) current lexbuf
  }
  | "}" | ")" as op {
    let current = push_character current op in
    match level with
    | nestop :: level when Nesting.of_closing op = nestop ->
       begin match level with
       | [] | (Nesting.DQuotes | Nesting.Backquotes _) :: _ ->
          current
       | _ ->
          next_nesting level current lexbuf
       end
    | _ :: _ ->
       lexing_error ("Unterminated " ^ Nesting.to_string (List.hd level)
                 ^ " got " ^ String.make 1 op)
    | [] ->
      assert false
      (* Because we maintain the invariant that [level] is non empty. *)

  }
  | '\'' {
    let current' = push_character current '\'' in
    if under_double_quotes level || escaped_single_quote level current then (
      next_nesting level current' lexbuf
    ) else
      let current = single_quotes current' lexbuf in
      next_nesting level current lexbuf
  }
  | '"' {
    let current' = push_character current '"' in
    if escaped_double_quote level current then (
       next_nesting level current' lexbuf
     ) else
      let level' = Nesting.DQuotes :: level in
      let current = double_quotes level' current' lexbuf in
      next_nesting level current lexbuf
  }

(**specification

  Within the backquoted style of command substitution,
  <backslash> shall retain its literal meaning, except when followed
  by: '$', '`', or <backslash>.

*)
  (* FIXME: The following rule seems redundant with the next one. *)
  (* | "\\" (_ as c) { *)
  (*   let current = *)
  (*     if under_backquoted_style_command_substitution level then *)
  (*       push_character current c *)
  (*     else *)
  (*       push_string current (Lexing.lexeme lexbuf) *)
  (*   in *)
  (*   let current = push_string current (Lexing.lexeme lexbuf) in *)
  (*   next_nesting level current lexbuf *)
  (* } *)

  | '\\' (_ as c) {
    let current =
      if under_backquoted_style_command_substitution level then
          match c with
            | '$' | '\\' | '`' ->
              push_character current c
            | c ->
              push_string current (Lexing.lexeme lexbuf)
      else
        push_string current (Lexing.lexeme lexbuf)
    in
    next_nesting level current lexbuf
  }
  (* FIXME: do we have to handle "\<newline" here ? *)
  | eof {
    lexing_error "Unterminated nesting."
  }
  | _ as c {
    let current = push_character current c in
    next_nesting level current lexbuf
  }

and next_double_rparen level dplevel current = parse
  | "((" {
    let current = push_string current "((" in
    next_double_rparen level (dplevel+1) current lexbuf
  }
  | '`' as op | "$" ( '(' as op) {
      let current =
        subshell op level (push_string current (Lexing.lexeme lexbuf)) lexbuf
      in
      let current =
        close_subshell current lexbuf
      in
      next_double_rparen level dplevel current lexbuf
  }
  | "))" {
    let current = push_string current "))" in
    if dplevel = 1
    then current
    else if dplevel > 1 then next_double_rparen level (dplevel-1) current lexbuf
    else assert false
  }
  | eof {
    lexing_error "Unterminated arithmetic expression."
  }
  | _ as c {
    next_double_rparen level dplevel (push_character current c) lexbuf
  }

(**specification

   2.2.2 Single-Quotes

   Enclosing characters in single-quotes ( '' ) shall preserve the
   literal value of each character within the single-quotes. A
   single-quote cannot occur within single-quotes.

*)
and single_quotes current = parse
  | '\'' {
    push_character current '\''
  }

(** Single quotes must be terminated before the end of file. *)
  | eof {
    lexing_error "Unterminated quote."
  }

(** Otherwise, we simply copy the character. *)
  | _ as c {
    single_quotes (push_character current c) lexbuf
  }

(**specification

2.2.3 Double-Quotes

   Enclosing characters in double-quotes ( "" ) shall preserve the
   literal value of all characters within the double-quotes, with the
   exception of the characters backquote, <dollar-sign>, and <backslash>,
   as follows:

*)
and double_quotes level current = parse
  | '"' {
    let is_escaped = escaped_double_quote level current in
    let current' = push_character current '"' in
    if is_escaped then (
      double_quotes level current' lexbuf
    )
    else
      current'
  }

(**specification

   $

   The <dollar-sign> shall retain its special meaning introducing
   parameter expansion (see Parameter Expansion), a form of command
   substitution (see Command Substitution), and arithmetic expansion
   (see Arithmetic Expansion).

   The input characters within the quoted string that are also
   enclosed between "$(" and the matching ')' shall not be affected by
   the double-quotes, but rather shall define that command whose
   output replaces the "$(...)" when the word is expanded. The
   tokenizing rules in Token Recognition, not including the alias
   substitutions in Alias Substitution, shall be applied recursively
   to find the matching ')'.

*)
  (* | "$(" { *)
  (*   (\* FIXME We should call a subshell parser here! *\) *)
  (*   let current = push_string current "$(" in *)
  (*   let current = next_nesting (Nesting.Parentheses :: level) current lexbuf in *)
  (*   double_quotes level current lexbuf *)
  (* } *)

(**specification

   Within the string of characters from an enclosed "${" to the
   matching '}', an even number of unescaped double-quotes or
   single-quotes, if any, shall occur. A preceding <backslash>
   character shall be used to escape a literal '{' or '}'. The rule in
   Parameter Expansion shall be used to determine the matching '}'.

*)
  | "${" {
    let current = push_string current "${" in
    let current = next_nesting (Nesting.Braces :: level) current lexbuf in
    double_quotes level current lexbuf
  }

(**specification

   `

   The backquote shall retain its special meaning introducing the
   other form of command substitution (see Command Substitution). The
   portion of the quoted string from the initial backquote and the
   characters up to the next backquote that is not preceded by a
   <backslash>, having escape characters removed, defines that command
   whose output replaces "`...`" when the word is expanded. Either of
   the following cases produces undefined results: A single-quoted or
   double-quoted string that begins, but does not end, within the
   "`...`" sequence

   A "`...`" sequence that begins, but does not end, within the same
   double-quoted string

*)
  | '`' as op | "$" ('(' as op) {
    let current =
      subshell op level (push_string current (Lexing.lexeme lexbuf)) lexbuf
    in
    let current =
      close_subshell current lexbuf
    in
    double_quotes level current lexbuf
  }

| "$((" {
    let current = push_string current "$((" in
    let current = next_double_rparen level 1 current lexbuf in
    double_quotes level current lexbuf
  }


(**specification

   \

   The <backslash> shall retain its special meaning as an escape
   character (see Escape Character (Backslash)) only when followed by
   one of the following characters when considered special:

      $   `   <double-quote>   \   <newline>

   The application shall ensure that a double-quote is preceded by a
   <backslash> to be included within double-quotes. The parameter '@'
   has special meaning inside double-quotes and is described in
   Special Parameters .

*)
  | "\\" ('$' | '`' | '"' | "\\" | newline as c) {
    if c = "\"" then begin
        let current = push_character current '\\' in
        if escaped_double_quote level current then
          let current = push_string current c in
          double_quotes level current lexbuf
        else
          let current' = push_string current c in
          current'
      end
    else
    double_quotes level (push_string current (Lexing.lexeme lexbuf)) lexbuf
  }

(** Double quotes must be terminated before the end of file. *)
  | eof {
    lexing_error "Unterminated double quote."
  }

(** Otherwise, we simply copy the current character. *)
  | _ as c {
    double_quotes level (push_character current c) lexbuf
  }

and readline = parse
  | eof {
    None
  }
  | [^ '\n' '\r']* (newline | eof) {
      let result =
        Some((Lexing.lexeme lexbuf),
             lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
      in
      Lexing.new_line lexbuf;
      result
  }
