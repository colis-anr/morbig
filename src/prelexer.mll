(** -*- tuareg -*-

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
open Parser

(* FIXME: The name "Word" is confusing here. It must be changed. *)
type pretoken =
  | Word of string
  | IoNumber of string
  | Operator of Parser.token
  | EOF
  | NEWLINE

let push_character b c =
  Buffer.add_char b c;
  b

let push_string b s =
  Buffer.add_string b s;
  b

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
    let s = Buffer.contents b in
    Buffer.clear b;
    s
  and produce token =
    (* FIXME: Positions are not updated properly. *)
    (token, lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)
  in
  let is_digit d =
    Str.(string_match (regexp "^[0-9]+$") d 0)
  in
  let followed_by_redirection = function
    | Operator (LESSAND |  GREATAND | DGREAT | CLOBBER | LESS | GREAT) :: _ ->
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

let optoken_of_string s = Operator (Hashtbl.find operators s)

type nesting =
  | Backquotes
  | Parentheses
  | Braces

let nesting_of_opening c =
  if c = '(' then Parentheses
  else if c = '{' then Braces
  else if c = '`' then Backquotes
  else failwith "Unrecognized nesting."

let nesting_of_closing c =
  if c = ')' then Parentheses
  else if c = '}' then Braces
  else if c = '`' then Backquotes
  else failwith "Unrecognized nesting."

let here_document_placeholder () =
  ref (CST.({
      value = Word "<you should not see this>";
      position = dummy_position
  }))

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

rule token current = parse

(**specification:

   If the end of input is recognized, the current token shall be
   delimited. If there is no current token, the end-of-input indicator
   shall be returned as the token.

*)
  | eof {
    return lexbuf current [EOF]
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
    token current lexbuf
         }
  | '\\' _  {
    token (push_string current (Lexing.lexeme lexbuf)) lexbuf
  }

(**specification

   2.2.2 Single-Quotes

*)
  | '\'' {
    let current = push_character current '\'' in
    token (single_quotes current lexbuf) lexbuf
  }


(** specification

    2.2.3 Double-Quotes

*)
  | '"' {
    let current = push_character current '"' in
    token (double_quotes current lexbuf) lexbuf
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
  | operator as s {
    return lexbuf current [optoken_of_string s]
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
    token (push_string current s) lexbuf
  }

  (* FIXME: Handle nesting *)
  | '`' as op | "$" ( ['{' '('] as op) {
    let current = push_string current (Lexing.lexeme lexbuf) in
    let current = next_nesting (nesting_of_opening op) 1 current lexbuf in
    token current lexbuf
  }
  | "$((" {
    let current = push_string current "$((" in
    let current = next_double_rparen 1 current lexbuf in
    token current lexbuf
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
  | '#' [^'\n''\r']* newline {
    return ~with_newline:true lexbuf current []
  }
  | '#' [^'\n''\r']* eof {
    return ~with_newline:false lexbuf current []
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
    after_equal [] current lexbuf
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
    token (push_character current c) lexbuf
  }

and after_equal level current = parse
  | "`" {
    let current = push_character current '`' in
    match level with
      | Backquotes :: level ->
        after_equal level current lexbuf
      | level ->
        after_equal (Backquotes :: level) current lexbuf
  }
  | "("|"{" as op {
    let current = push_character current op in
    after_equal (nesting_of_opening op :: level) current lexbuf
  }
  | ")"|"}" as op {
    let current = push_character current op in
    match level with
      | nestop :: level when nestop=nesting_of_closing op ->
        after_equal level current lexbuf
      | _ ->
        failwith "Lexing error: unbalanced parantheses"
  }
  | "\"" {
    let current = push_character current '"' in
    after_equal level (double_quotes current lexbuf) lexbuf
  }
  | "\'" {
    let current = push_character current '\'' in
    after_equal level (single_quotes current lexbuf) lexbuf
  }
  | '\\' _ {
    after_equal level (push_string current (Lexing.lexeme lexbuf)) lexbuf
  }
  (* FIXME: Factorize the following two rules. *)
  | newline {
    if level = [] then
      return ~with_newline:true lexbuf current []
    else (
      let current = push_character current ' ' in
      after_equal level current lexbuf
    )
  }
  | blank {
    if level = [] then
      return ~with_newline:false lexbuf current []
    else (
      let current = push_character current ' ' in
      after_equal level current lexbuf
    )
  }
  (* FIXME: which other operators shall be accepted as delimiters here ?*)
  | (";;" | ";") as s {
    return lexbuf current [optoken_of_string s]
  }
  | _ as c {
    after_equal level (push_character current c) lexbuf
  }
  | eof {
    failwith "Unterminated assignment."
  }

and next_nesting nestop level current = parse
  | '`' as op {
    if nestop=Backquotes
    then push_character current op
    else next_nesting nestop level (push_character current op) lexbuf
  }
  | "{" | "(" as op {
    let current = push_character current op in
    next_nesting
      nestop
      (if nesting_of_opening op = nestop
       then level+1
       else level)
      current
      lexbuf
  }
  | "}" | ")" as op {
    let current = push_character current op in
    if  nesting_of_closing op = nestop
    then
      if level=1 then current
      else if level>1 then next_nesting nestop (level-1) current lexbuf
      else assert false
    else next_nesting nestop level current lexbuf
  }
  | '\'' {
    let current = push_character current '\'' in
    next_nesting nestop level (single_quotes current lexbuf) lexbuf
  }
  | '"' {
    let current = push_character current '"' in
    next_nesting nestop level (double_quotes current lexbuf) lexbuf
  }
  | '\\' _ {
    next_nesting nestop level
    (push_string current (Lexing.lexeme lexbuf)) lexbuf
  }
  (* FIXME: do we have to handle "\<newline" here ? *)
  | eof {
    failwith "Unterminated nesting."
  }
  | _ as c {
    next_nesting nestop level (push_character current c) lexbuf
  }

and next_double_rparen level current = parse
  | "((" {
    let current = push_string current "((" in
    next_double_rparen (level+1) current lexbuf
  }
  | "))" {
    let current = push_string current "))" in
    if level=1
    then current
    else if level>1 then next_double_rparen (level-1) current lexbuf
    else assert false
  }
  | eof {
    failwith "Unterminated arithmetic expression."
  }
  | _ as c {
    next_double_rparen level (push_character current c) lexbuf
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
    failwith "Unterminated quote."
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
and double_quotes current = parse
  | '"' {
    push_character current '"'
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
    let current = push_string current "$(" in
    let current = next_nesting Parentheses 1 current lexbuf in
    double_quotes current lexbuf
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
    let current = next_nesting Braces 1 current lexbuf in
    double_quotes current lexbuf
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
  | "`" {
    let current = push_string current (Lexing.lexeme lexbuf) in
    let current = next_nesting Backquotes 1 current lexbuf in
    double_quotes current lexbuf
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
  | "\\" ('$' | '`' | '"' | "\\" | newline) {
    double_quotes (push_string current (Lexing.lexeme lexbuf)) lexbuf
  }

(** Double quotes must be terminated before the end of file. *)
  | eof {
    failwith "Unterminated double quote."
  }

(** Otherwise, we simply copy the current character. *)
  | _ as c {
    double_quotes (push_character current c) lexbuf
  }

and readline = parse
  | eof {
    None
  }
  | [^ '\n' '\r']* (newline | eof) {
    Some((Lexing.lexeme lexbuf),
	  lexbuf.Lexing.lex_start_p,lexbuf.Lexing.lex_curr_p)
  }
