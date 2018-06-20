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
open Lexing
open ExtPervasives
open Parser
open CST
open PrelexerState
open Pretoken

let subshell_parsing op escaping_level level lexbuf =
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
    let level = Nesting.Backquotes (op, escaping_level) :: level in
    let subshell_kind =
      match op with
      | '`' -> SubShellKindBackQuote
      | '(' -> SubShellKindParentheses
      | _ -> assert false (* By usage of [subshell_parsing]. *)
    in
    let cst = (!RecursiveParser.parse) level lexbuf' in
    let consumed_characters =
      lexbuf'.Lexing.lex_curr_p.Lexing.pos_cnum
      - lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum
    in
    (consumed_characters, (subshell_kind, cst))

let lexing_error lexbuf msg =
  raise (Errors.LexicalError (lexbuf.Lexing.lex_curr_p, msg))

}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

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

(**

   The lexing rule [token] is parameterized by:

   - [level] of type [Nesting.t list] which represents the context under
     which lexical analysis is performed.

   - [current] of type [PrelexerState.t] which represents the state of
     the lexical engine.

*)
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
      lexing_error lexbuf "Unterminated nesting!"
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
    Lexing.new_line lexbuf;
    (**
        Notice that we do not push <newline> in the prelexer state
        since it must be ignored as specified above.
    *)
    token level current lexbuf
  }

  | '\\' (_ as c) {
    if under_backquoted_style_command_substitution level then
      match c with
      | '$' | '\\' ->
         let current = push_character current c in
         token level current lexbuf
      | '`' ->
         begin match escaped_backquote level current with
         | None ->
            let current = push_character current '\\' in
            let current = push_character current c in
            token level current lexbuf
         | Some escaping_level ->
            match list_hd_opt level with
            | Some (Nesting.Backquotes ('`', escaping_level')) ->
               (* FIXME: Do we have to be finer here by looking at the
                  order between escaping levels? *)
               if escaping_level' = escaping_level then
                 (* This is a closing backquote. *)
                 let pos = lexbuf.lex_curr_p.pos_cnum in (
                     lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum = pos - 1 };
                     return lexbuf current [EOF]
                   )
               else
                 (* This is an opening backquote. *)
                 let current = push_separated_string current (Lexing.lexeme lexbuf) in
                 let current = subshell '`' escaping_level level current lexbuf in
                 let current = close_subshell current lexbuf in
                 token level current lexbuf
            | None ->
                 (* This is an opening backquote. *)
                 let current = push_separated_string current (Lexing.lexeme lexbuf) in
                 let current = subshell '`' escaping_level level current lexbuf in
                 let current = close_subshell current lexbuf in
                 token level current lexbuf
            | Some (Nesting.Backquotes ('(', _)
                           | Nesting.Parentheses
                           | Nesting.Braces
                           | Nesting.DQuotes)
              ->
               (* FIXME: Not sure what to do here... *)
               assert false (* TODO *)
            | _ ->
               assert false (* By usage of Backquotes. *)
                      (* FIXME: We should introduce a finer type for backquotes. *)
         end
      | '"' ->
         if escaped_double_quote level current then
           let current = push_character current '\\' in
           let current = push_character current c in
           token level current lexbuf
         else (
           let current = push_quoting_mark SingleQuote current in
           let current = double_quotes (Nesting.DQuotes :: level) current lexbuf in
           let current = pop_quotation DoubleQuote current in
           token level current lexbuf
         )
      | c ->
         let current = push_string current (Lexing.lexeme lexbuf) in
         token level current lexbuf
    else
      let current = push_string current (Lexing.lexeme lexbuf) in
      token level current lexbuf
  }

(**specification

   2.2.2 Single-Quotes

*)
  | '\'' {
    if escaped_single_quote level current then (
      token level (push_character current '\'') lexbuf
    ) else
      let current = push_character current '\'' in
      let current = push_quoting_mark SingleQuote current in
      let current = single_quotes current lexbuf in
      let current = pop_quotation SingleQuote current in
      token level current lexbuf
  }


(** specification

    2.2.3 Double-Quotes

*)
  | '"' {
    if escaped_double_quote level current then
      token level current lexbuf
    else
      let current = push_quoting_mark DoubleQuote current in
      let current = double_quotes (Nesting.DQuotes :: level) current lexbuf in
      let current = pop_quotation DoubleQuote current in
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
    let placeholder = CSTHelpers.word_placeholder () in
    return lexbuf current [Operator (DLESS placeholder)]
  }
  | "<<-" {
    let placeholder = CSTHelpers.word_placeholder () in
    return lexbuf current [Operator (DLESSDASH placeholder)]
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

  (* FIXME: The following treatment of # is probably subsumed by
     FIXME: the more general rule about '#' below. *)
  (* $# is a special parameter, that is a # after an $ does not start a
     comment *)
  | ("$" "#"?) as s {
    token level (push_string current s) lexbuf
  }

  | '`' as op | "$" ('(' as op) {
    let is_under_backquote level =
      match list_hd_opt level with
      | Some (Nesting.Backquotes ('`', _)) -> true
      | _ -> false
    in
    if op = '`' && is_under_backquote level then begin
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
      let escaping_level = 0 in
      let current = push_separated_string current (Lexing.lexeme lexbuf) in
      let current = subshell op escaping_level level current lexbuf in
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
    failwith "TODO"
  }

(**specification:

  If the current character is an unquoted <newline>, the current
  token shall be delimited.

*)
  | newline {
    Lexing.new_line lexbuf;
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
    if current.buffer = [] then
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
    let current = push_character current c in
    let current = push_assignment_mark current in
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
| eof {
  assert false (* By subshell_parsing. *)
}

| _ as c {
  let current = push_character current c in
  if k <= 1 then current else skip (k - 1) current lexbuf
}

and close op = parse
| "`" {
  if op <> '`' then lexing_error lexbuf "Lexing error: unbalanced backquotes."
}
| (")" | "}" as op') {
  if (op = '(' && op' <> ')') || (op = '{' && op' <> '}') then
    lexing_error lexbuf (Printf.sprintf "Lexing error: unbalanced $%c." op)
}
| _ as c {
  lexing_error lexbuf (Printf.sprintf "Unclosed %c (got %c)." op c)
}

and comment = parse
| [^'\n''\r']* newline {
    Lexing.new_line lexbuf;
    return ~with_newline:true lexbuf initial_state []
  }
| '#' [^'\n''\r']* eof {
    return ~with_newline:false lexbuf initial_state []
  }


and subshell op escaping_level level current = parse
  | "" {
    let (consumed, (k, cst)) = subshell_parsing op escaping_level level lexbuf in
    let current =
      if consumed > 0 then skip consumed current lexbuf else current
    in
    let subshell_strings, current =
      ExtPervasives.take (List.length cst) current.buffer
    in
    let subshell_string =
      String.concat "" (
          List.rev_map (function
              | WordComponent (w, _) -> w
              | AssignmentMark -> ""
              | _ -> assert false
            ) subshell_strings)
    in
    let subshell =
      WordComponent (subshell_string, WordSubshell (k, cst))
    in
    { buffer = subshell :: current }
  }

and close_subshell current = parse
  | (")" | "`") as c {
     push_word_closing_character current c
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
     lexing_error lexbuf (Printf.sprintf "Unclosed subshell (got EOF).")
  }
  | _ as c {
     lexing_error lexbuf (Printf.sprintf "Unclosed subshell (got '%c')." c)
  }

and return_subshell op escaping_level level current = parse
 | "" {
    let current = subshell op escaping_level level current lexbuf in
    let current = close_subshell current lexbuf in
    return lexbuf current []
 }

and after_equal level current = parse
  | '`' as op | "$" ( '(' as op) {
    let is_under_backquote level =
      match list_hd_opt level with
      | Some (Nesting.Backquotes (op, _)) -> true
      | _ -> false
    in
    if op = '`' && is_under_backquote level then
     (*

         If the last nesting symbol of [level] is a Backquote, the
         current backquote is the closing symbol.

     *)
      provoke_error current lexbuf
    else
      let escaping_level = 0 in (* FIXME: Probably wrong. *)
      let current = push_separated_string current (Lexing.lexeme lexbuf) in
      let current = subshell op escaping_level level current lexbuf in
      let current = close_subshell current lexbuf
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
      | Nesting.Backquotes ('(', _) :: level when op = ')' ->
         provoke_error current lexbuf
      | Nesting.Backquotes ('`', _) :: level when op = '`' ->
         (* FIXME: Maybe wrong *)
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
         lexing_error lexbuf (Printf.sprintf
                     "Lexing error: unbalanced parentheses (%c <> %s)"
                   op x)
    end
  }
  | "\"" {
    let is_escaped = escaped_double_quote level current in
    let current =
      if not is_escaped then
        let current = push_quoting_mark DoubleQuote current in
        let current = double_quotes (Nesting.DQuotes :: level) current lexbuf in
        pop_quotation DoubleQuote current
      else
        current
    in
    after_equal level current lexbuf
  }
  | "\'" {
    (* FIXME: Factorize this out, with the case in [token]. *)
    if escaped_single_quote level current then (
      after_equal level current lexbuf
    ) else
      let current = push_character current '\'' in
      let current = push_quoting_mark SingleQuote current in
      let current = single_quotes current lexbuf in
      let current = pop_quotation SingleQuote current in
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

and next_double_rparen level dplevel current = parse
  | "((" {
    let current = push_string current "((" in
    next_double_rparen level (dplevel+1) current lexbuf
  }
  | '`' as op | "$" ( '(' as op) {
    let escaping_level = 0 in (* FIXME: Probably wrong. *)
    let current = push_string current (Lexing.lexeme lexbuf) in
    let current = subshell op escaping_level level current lexbuf in
    let current = close_subshell current lexbuf in
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
    lexing_error lexbuf "Unterminated arithmetic expression."
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
    lexing_error lexbuf "Unterminated quote."
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
  | "$(" {
    failwith "TODO"
  }

(**specification

   Within the string of characters from an enclosed "${" to the
   matching '}', an even number of unescaped double-quotes or
   single-quotes, if any, shall occur. A preceding <backslash>
   character shall be used to escape a literal '{' or '}'. The rule in
   Parameter Expansion shall be used to determine the matching '}'.

*)
  | "${" {
    failwith "TODO"
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
    let escaping_level = 0 in (* FIXME: Check this. *)
    let current = push_separated_string current (Lexing.lexeme lexbuf) in
    let current = subshell op escaping_level level current lexbuf in
    let current = close_subshell current lexbuf
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
    lexing_error lexbuf "Unterminated double quote."
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

{
  let token level = token level initial_state
}
