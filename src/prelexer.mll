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

let push_current_string current lexbuf continue =
  let current = push_string current (Lexing.lexeme lexbuf) in
  continue current lexbuf

let if_unprotected_by_double_quote_or_braces current lexbuf default f =
    if PrelexerState.(under_double_quote current || under_braces current) then
      push_current_string current lexbuf default
    else
      f ()

let if_unprotected_by_double_quote current lexbuf default f =
    if PrelexerState.(under_double_quote current) then (
      push_current_string current lexbuf default
    ) else
      f ()

let if_ escaped current lexbuf do_this or_else =
  if escaped current then
    push_current_string current lexbuf do_this
  else
    or_else ()

let rewind current lexbuf f =
  let pos = lexbuf.lex_curr_p.pos_cnum in
  let bol = lexbuf.lex_curr_p.pos_bol in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with
                         pos_cnum = pos - 1;
                         pos_bol = bol - 1
                       };
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - 1;
  debug ~rule:"rewind" lexbuf current;
  f current lexbuf

let subshell_parsing op escaping_level current lexbuf =
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
    let current = enter_backquotes op escaping_level current in
    let subshell_kind =
      match op with
      | '`' -> SubShellKindBackQuote
      | '(' -> SubShellKindParentheses
      | _ -> assert false (* By usage of [subshell_parsing]. *)
    in
    let current' =
      { initial_state with nesting_context = current.nesting_context }
    in
    let cst = (!RecursiveParser.parse) current' lexbuf' in
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

let alpha = ['a'-'z' 'A'-'Z' '_']

let digit = ['0'-'9']

let name = alpha (alpha | digit)*

let special_character = "@" | "*" | "#" | "?" | "-" | "$" | "!" | "0"

let parameter_identifier = name | digit | special_character

let quote_removal_special_characters = "$" | "\\" | "`" | "\""

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

   The lexing rule [token] is parameterized by [current] of type
   [PrelexerState.t] which represents the state of the lexical
   engine.

   As described in [PrelexerState], the state of the pretokenizer
   contains contextual information. More precisely, the lexical
   analysis needs to know:

   - the "nesting context", roughly speaking, is the imbrication
     of double-quotes and backquotes in the whole input (in particular,
     it includes the nesting context of parent parsers, i.e. parsers
     that recursively called the current instance for subshell parsing) ;

   - the "lexing context" is a state that constraints lexical rules
     (for instance, being on the right-hand-side of an <equal> character
     may lead to the production of an assignment word, while other
     context may not) ;

   - the "buffer" is a sequence of input items that are still to be
     included in a pretoken.

*)
rule token current = parse

  | eof {
    debug ~rule:"eof" lexbuf current;
    (** The end of file cannot occur inside a nested construction. *)
    if at_toplevel current then
      (**specification:

         If the end of input is recognized, the current token shall be
         delimited. If there is no current token, the end-of-input
         indicator shall be returned as the token.

      *)
      if found_current_here_document_delimiter current then
        return lexbuf current [EOF]
      else
        return lexbuf current [EOF]
    else
      lexing_error lexbuf "Unterminated nesting!"
  }

(** Quotations *)

(**specification:

   If the current character is <backslash>, single-quote, or
   double-quote and it is not quoted, it shall affect quoting for
   subsequent characters up to the end of the quoted text. The rules for
   quoting are as described in Quoting. During token recognition no
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
    token current lexbuf
  }

  | ('\\'+ as backslashes) (_ as c) {
    debug ~rule:"backslash" lexbuf current;
    (**
        We have to decide if the <backslash>es have an
        escaping power. This depends on the nesting context.
    *)
    let current' = push_string current backslashes in
    if PrelexerState.is_escaping_backslash current' lexbuf c then
      (**
          Yes, this <backslash> preserves the literal value of the following
          character as demanded by the POSIX standard.
       *)
      push_current_string current lexbuf @@ token
    else (
      (**
          Otherwise, the <backslash> has no effect on [c]. We reinject
          the character [c] in the input to analyze it separately.
      *)
      rewind current' lexbuf @@ token
    )
  }

(**specification

   2.2.2 Single-Quotes

*)
| '\'' {
    debug ~rule:"single-quote" lexbuf current;
    if_unprotected_by_double_quote current lexbuf token (fun () ->
      let current = push_quoting_mark SingleQuote current in
      let current = single_quotes current lexbuf in
      let current = pop_quotation SingleQuote current in
      token current lexbuf
     )
  }

(** specification

    2.2.3 Double-Quotes

*)
| '"' {
  debug ~rule:"double-quote" lexbuf current;
  if_ PrelexerState.escaped_double_quote current lexbuf token (fun () ->
    let open_double_quote current =
      let current = push_quoting_mark DoubleQuote current in
      let current = enter_double_quote current in
      token current lexbuf
    in
    let close_double_quote current =
      let current = pop_quotation DoubleQuote current in
      let current = quit_double_quote current in
      token current lexbuf
    in
    if PrelexerState.under_real_double_quote current then
      close_double_quote current
    else
      open_double_quote current
  )
}

(** Backquotes *)
| "`" {
  if_ PrelexerState.escaped_backquote current lexbuf token (fun () ->
      debug ~rule:"backquote" lexbuf current;
      let open_subshell depth current =
        let current = push_separated_string current (Lexing.lexeme lexbuf) in
        let current = subshell '`' depth current lexbuf in
        let current = close_subshell current lexbuf in
        token current lexbuf
      in
      let end_of_subshell current =
        rewind current lexbuf @@ fun c l -> return l c [EOF]
      in
      match PrelexerState.backquote_depth current with
      | Some depth ->
         (** There is new subshell nesting. *)
         open_subshell depth current
      | None ->
         (** We were already in a subshell, this is the end of it. *)
         end_of_subshell current
    )
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
    if_unprotected_by_double_quote current lexbuf token (fun () ->
      let placeholder = CSTHelpers.word_placeholder () in
      return lexbuf current [Operator (DLESS placeholder)]
    )
  }
  | "<<-" {
    if_unprotected_by_double_quote current lexbuf token (fun () ->
      let placeholder = CSTHelpers.word_placeholder () in
      return lexbuf current [Operator (DLESSDASH placeholder)]
    )
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

  | "$" ('(' as op) {
    debug ~rule:"paren-intro-subshell" lexbuf current;
    if op = '`' && under_backquote current then begin
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
      let current = subshell op escaping_level current lexbuf in
      let current = close_subshell current lexbuf in
      token current lexbuf
  }

| operator as s {
    debug ~rule:"operator" lexbuf current;
    if_unprotected_by_double_quote current lexbuf token (fun () ->
      let operator = optoken_of_string s in
      return lexbuf current [operator]
    )
  }

| "$((" {
    let current = push_string current "$((" in
    let current = next_double_rparen 1 current lexbuf in
    token current lexbuf
  }

(**specification

   If the parameter is not enclosed in braces, and is a name, the
   expansion shall use the longest valid name (see XBD Name), whether
   or not the variable represented by that name exists. Otherwise, the
   parameter is a single-character symbol, and behavior is unspecified
   if that character is neither a digit nor one of the special
   parameters (see Special Parameters).

 *)
| "$" (parameter_identifier as id) {
  debug ~rule:"parameter-with-no-braces" lexbuf current;
  let current = push_parameter current id in
  token current lexbuf
}

(**specification

   Within the string of characters from an enclosed "${" to the
   matching '}', an even number of unescaped double-quotes or
   single-quotes, if any, shall occur. A preceding <backslash>
   character shall be used to escape a literal '{' or '}'. The rule in
   Parameter Expansion shall be used to determine the matching '}'.

*)
  | "${" (parameter_identifier as id) {
  debug ~rule:"parameter-opening-braces" lexbuf current;
  let current = enter_braces current in
  let attribute = close_parameter current lexbuf in
  let current = quit_braces current in
  let current = push_parameter ~with_braces:true ~attribute current id in
  token current lexbuf
}

| "}" {
  debug ~rule:"parameter-closing-brace" lexbuf current;
  if under_braces current then
    let current = pop_quotation OpeningBrace current in
    let _ =   debug ~rule:"parameter-closing-brace-after-pop" lexbuf current in
    return lexbuf current []
  else
    push_current_string current lexbuf @@ token
}

(**specification:

  If the current character is an unquoted <newline>, the current
  token shall be delimited.

*)
  | newline {
    Lexing.new_line lexbuf;
    if found_current_here_document_delimiter current then
      return ~with_newline:true lexbuf current []
    else if_unprotected_by_double_quote current lexbuf token (fun () ->
      return ~with_newline:true lexbuf current []
    )
  }

(**specification:

  If the current character is an unquoted <blank>, any token
  containing the previous character is delimited and the current
  character shall be discarded.

*)
  | blank {
    debug ~rule:"blank" lexbuf current;
    if_unprotected_by_double_quote_or_braces current lexbuf token (fun () ->
        return lexbuf current []
    )
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
    if_unprotected_by_double_quote_or_braces current lexbuf token (fun () ->
      (**

         There two cases depending on the characters on the left of '#':
         If '#' is preceded by a separator, it is starting a comment.
         otherwise, '#' is part of a word.

      *)
      if current.buffer = [] then
        comment lexbuf
      else
        token (push_character current c) lexbuf
    )
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
(* FIXME: We shall issue a warning when we are in the unspecified case. *)
  | ((name as id) '=') as input {
    debug ~rule:"assignment" lexbuf current;
    (* FIXME: Check that "name" is preceded by a delimiter. *)
    let current = push_string current input in
    let current = push_assignment_mark current in
    let current = enter_assignment_rhs current (Name id) in
    token current lexbuf
  }

(**specification:

   If the previous character was part of a word, the current character
   shall be appended to that word.

*)

(**specification:

  The current character is used as the start of a new word.

*)
  (* FIXME: can we really accept *anything* here ? *)
  | _ as c {
    debug ~rule:"generic" lexbuf current;
    token (push_character current c) lexbuf
  }

and close_parameter current = parse
| "}" {
  debug ~rule:"close-parameter-closing-brace" lexbuf current;
  (** The word is not here. *)
  NoAttribute
}
| ":"?"-" {
  UseDefaultValues (variable_attribute current lexbuf)
}
| ":"?"=" {
  AssignDefaultValues (variable_attribute current lexbuf)
}
| ":"?"?" {
  IndicateErrorifNullorUnset (variable_attribute current lexbuf)
}
| ":"?"+" {
  UseAlternativeValue (variable_attribute current lexbuf)
}
| "%" {
  RemoveSmallestSuffixPattern (variable_attribute current lexbuf)
}
| "%%" {
  RemoveLargestSuffixPattern (variable_attribute current lexbuf)
}
| "#" {
  RemoveSmallestPrefixPattern (variable_attribute current lexbuf)
}
| "##" {
  RemoveLargestPrefixPattern (variable_attribute current lexbuf)
}

and variable_attribute current = parse
| "" {
  let current =
    { initial_state with nesting_context = current.nesting_context }
  in
  let current = push_quoting_mark OpeningBrace current in
  match token current lexbuf with
  | [] ->
     (** Null attribute. *)
     Word ("", [WordEmpty])
  | prewords ->
     (** Not null, must be unique. *)
     word_of prewords
}

and eat c = parse
| _ as c' {
    if c <> c' then
      lexing_error lexbuf (Printf.sprintf "Expecting `%c'." c)
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


and subshell op escaping_level current = parse
  | "" {
    let (consumed, (k, cst)) =
      subshell_parsing op escaping_level current lexbuf
    in
    let current =
      if consumed > 0 then skip consumed current lexbuf else current
    in
    PrelexerState.debug ~rule:"subshell" lexbuf current;
    let subshell_string, buffer =
      match current.buffer with
      | WordComponent (w, _) :: buffer -> w, buffer
      | _ -> assert false
    in
    let subshell =
      WordComponent (subshell_string, WordSubshell (k, cst))
    in
    { current with buffer = subshell :: buffer }
  }

and close_subshell current = parse
  | (")" | "`") as c {
     debug ~rule:"close_subshell:closing-char" lexbuf current;
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

and next_double_rparen dplevel current = parse
  | "((" {
    let current = push_string current "((" in
    next_double_rparen (dplevel+1) current lexbuf
  }
  | '`' as op | "$" ( '(' as op) {
    let escaping_level = 0 in (* FIXME: Probably wrong. *)
    let current = push_string current (Lexing.lexeme lexbuf) in
    let current = subshell op escaping_level current lexbuf in
    let current = close_subshell current lexbuf in
    next_double_rparen dplevel current lexbuf
  }
  | "))" {
    let current = push_string current "))" in
    if dplevel = 1
    then current
    else if dplevel > 1 then next_double_rparen (dplevel-1) current lexbuf
    else assert false
  }
  | eof {
    lexing_error lexbuf "Unterminated arithmetic expression."
  }
  | _ as c {
    next_double_rparen dplevel (push_character current c) lexbuf
  }

(**specification

   2.2.2 Single-Quotes

   Enclosing characters in single-quotes ( '' ) shall preserve the
   literal value of each character within the single-quotes. A
   single-quote cannot occur within single-quotes.

*)
and single_quotes current = parse
  | '\'' {
    if under_here_document current then
      push_current_string current lexbuf @@ single_quotes
    else
      current
  }

(** Single quotes must be terminated before the end of file. *)
  | eof {
    lexing_error lexbuf "Unterminated quote."
  }

  | newline {
    Lexing.new_line lexbuf;
    if found_current_here_document_delimiter current then
      current
    else
      push_current_string current lexbuf @@ single_quotes
  }

(** Otherwise, we simply copy the character. *)
  | _ as c {
    single_quotes (push_character current c) lexbuf
  }
