(**************************************************************************)
(*  Copyright (C) 2017 Yann Régis-Gianas, Nicolas Jeannerod,              *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*  The complete license terms can be found in the file COPYING.          *)
(**************************************************************************)

open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open MenhirLib.General
open ExtPervasives
open CST

(** Raise in case of parsing error. *)
exception ParseError

(**specification

   3.231 Name

   In the shell command language, a word consisting solely of
   underscores, digits, and alphabetics from the portable character
   set. The first character of a name is not a digit.

   Note:
   The Portable Character Set is defined in detail in Portable Character Set.

*)
let is_name s =
  Str.(string_match (
    regexp "^\\([a-zA-Z]\\|_\\)\\([a-zA-Z]\\|_\\|[0-9]\\)*$") s 0)

(**specification

   /* The following are the reserved words. */


   %token  If    Then    Else    Elif    Fi    Do    Done
   /*      'if'  'then'  'else'  'elif'  'fi'  'do'  'done'   */


   %token  Case    Esac    While    Until    For
   /*      'case'  'esac'  'while'  'until'  'for'   */

   /* These are reserved words, not operator tokens, and are
      recognized when reserved words are recognized. */


   %token  Lbrace    Rbrace    Bang
   /*      '{'       '}'       '!'   */


   %token  In
   /*      'in'   */

*)
let keywords = [
    "if",    If,     X (T T_If);
    "then",  Then,   X (T T_Then);
    "else",  Else,   X (T T_Else);
    "elif",  Elif,   X (T T_Elif);
    "fi",    Fi,     X (T T_Fi);
    "do",    Do,     X (T T_Do);
    "done",  Done,   X (T T_Done);
    "case",  Case,   X (T T_Case);
    "esac",  Esac,   X (T T_Esac);
    "while", While,  X (T T_While);
    "until", Until,  X (T T_Until);
    "for",   For,    X (T T_For);
    "{",     Lbrace, X (T T_Lbrace);
    "}",     Rbrace, X (T T_Rbrace);
    "!",     Bang,   X (T T_Bang);
    "in",    In,     X (T T_In);
]

let keyword_of_string =
  let t = Hashtbl.create 13 in
  List.iter (fun (s, kwd, _) -> Hashtbl.add t s kwd) keywords;
  Hashtbl.find t

let is_reserved_word w =
  try ignore (keyword_of_string w); true with _ -> false

let terminal_of_keyword k =
  let (_, _, t) = List.find (fun (_, k', _) -> k = k') keywords in
  t

(**specification

   2.6.7 Quote Removal

   The quote characters ( <backslash>, single-quote, and double-quote)
   that were present in the original word shall be removed unless they
   have themselves been quoted.

*)

(** [remove_quote s] yields a copy of string [s], with all quotes removed
    as described in the specification. *)
let remove_quotes s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  let keep () = Buffer.add_char b s.[!i]; incr i
  and skip () = incr i in
  while !i<n do
    if s.[!i] = '\''
    then begin
        (* skip the initial single quote *)
        skip ();
        (* scan and push on the buffer until next single quote *)
        while (!i<n && s.[!i] <> '\'') do
          keep ()
        done;
        (* skip the final single quote *)
        if !i<n then skip ()
      end
    else if s.[!i] = '"'
    then
      (* just skip any double quote if we see it here (that is, not escaped
           and not inside single quotes *)
      skip ()
    else if s.[!i] = '\\'
    then begin
        (* skip the backslash *)
        skip ();
        (* and push the next symbol on the buffer *)
        if !i<n then keep ()
      end
    else keep ()
  done;
  Buffer.contents b

(** [untab s] returns a copy of s, without any leading TABs *)
let untab s =
  let len = String.length s in
  let rec number_tabs_from i s =
    if i >= len
    then len
    else
      if String.get s i = '\t'
      then number_tabs_from (i+1) s
      else i
  in
  let nt = number_tabs_from 0 s in
  String.sub s nt (len-nt)

(** [strip s] returns a copy of s, without any final newline *)
let strip s =
  let n = String.length s in
  if n > 0
  then let lastchar = s.[n-1] in
       if lastchar = '\n' || lastchar = '\r'
       then String.sub s 0 (n-1)
       else s
  else s

let current_items parsing_state =
  match Lazy.force (stack parsing_state) with
    | Nil ->
      []
    | Cons (Element (s, _, _, _), _) ->
      items s

let rec close checkpoint =
  match checkpoint with
    | AboutToReduce (_, _) -> close (resume checkpoint)
    | Rejected | HandlingError _ -> false
    | Accepted _ | InputNeeded _ | Shifting _ -> true

let accepted_token checkpoint token =
  match checkpoint with
    | InputNeeded _ -> close (offer checkpoint token)
    | _ -> false

let recognize_reserved_word_if_relevant checkpoint (pretoken, pstart, pstop) w =
  FirstSuccessMonad.(
    try
      let kwd = keyword_of_string w in
      if accepted_token checkpoint (kwd, pstart, pstop) then
        return kwd
      else
        raise Not_found
    with Not_found ->
      if is_name w then
        return (NAME (CST.Name w))
      else
        return (WORD (CST.Word w))
  )

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

   Assignment to the NAME shall occur as specified in Simple Commands.

*)

let recognize_assignment checkpoint pretoken w = FirstSuccessMonad.(
  match Str.(split_delim (regexp "=") w) with
    | [w] ->
      fail
    | [""; w] ->
      return (WORD (CST.Word ("=" ^ w)))
    | name :: rhs ->
      let rhs = String.concat "=" rhs in
      if is_name name then
        let aword = CST.(AssignmentWord (Name name, Word rhs)) in
        let (_, pstart, pstop) = pretoken in
        let token = ASSIGNMENT_WORD aword in
        if accepted_token checkpoint (token, pstart, pstop) then
          return token
        else
          return (WORD (CST.Word w))
      else
        (* We choose to return a WORD. *)
        return (WORD (Word w))
    | _ ->
      return (WORD (Word w))
)

(** [finished checkpoint] is [true] if the current [checkpoint] can
    move the LR(1) automaton to an accepting state with no extra
    input.
*)
let rec finished = function
  | Accepted _ -> true
  | (AboutToReduce (_, _) | Shifting (_, _, _)) as checkpoint ->
    finished (resume checkpoint)
  | _ -> false

(**

   [parse filename] parses each complete shell command of
   [filename] and returns a list of concrete syntax trees that
   represent them.

   Contrary to what is found in textbooks about parser architecture,
   the lexing (i.e. token recognition) highly depends on the parsing
   state. To take this aspect into account, the parser is decomposed
   into three components:

   - {!Prelexer}, an standard ocamllex-generated lexical analyzer that
   splits the input characters into a sequence of pretokens, namely
   words, operators, newlines and end-of-file marker.

   - [next_token] is a function that uses the current state of the
   parser to turn the current pretoken as a real token.

   - [parse] is an incremental step-wise LR(1) parser
   generated by menhir. Contrary to the textbook architecture where
   communication between the parser and the lexer is unidirectional
   (from the lexer to the parser), [parse] communicates
   its current state (represented by [checkpoint]) to the function
   [next_token].

   To complete the description of [parse_file], we must say that
   a preprocessing is applied to the input. This preprocessing
   is dedicated to the handling of line continuation. When a
   <backslash> is the last character of a line, this <backslash>
   and the end-of-line is removed from the input.

*)
let parse contents =

  (**-----------------------------------------------**)
  (** Preprocessing step: line continuation removal. *)
  (**-----------------------------------------------**)

  let lexbuf = Lexing.from_string contents in

  (**--------------------------**)
  (** {!Prelexer} pretokenizer. *)
  (**--------------------------**)

  let next_pretoken, push_pretoken =
    let pretokenizer = Prelexer.token [] in

    (** The pretokenizer may produce several pretokens, we
        use an intermediate queue to synchronize pretokens'
        consumption with their production. *)
    let q = Queue.create () in
    let push x = Queue.push x q in
    let rec aux () =
      try
        Queue.take q
      with Queue.Empty ->
        List.iter (fun x -> Queue.push x q) (pretokenizer lexbuf);
        aux ()
    in
    aux, push
  in

  (**---------------------**)
  (** Parsing-aware lexer. *)
  (**---------------------**)

  (** Once end-of-command has been reached, the lexer must return an
      end-of-file token each time it is subsequently called. The
      following boolean accounts for this two-states mechanism. *)
  let eof = ref false in
  let real_eof = ref false in

  (** The lexer works in two modes: either it is recognizing a
      here-document, or it is recognizing tokens as defined in
      the shell grammar. *)
  let here_document_on_next_line   = ref false
  and here_document_lexing         = ref false
  and here_document_delimiters     = ref []
  and here_document_skip_tabs      = ref []
  and here_document_find_delimiter = ref false
  and here_document_placeholders   = ref []
  in

  let fill_next_here_document_placeholder here_document =
    assert (!here_document_placeholders <> []);
    (List.hd !here_document_placeholders) := here_document;
    here_document_placeholders := List.tl !here_document_placeholders
  in

  let next_here_document () =
    assert (!here_document_delimiters <> []);
    assert (!here_document_skip_tabs <> []);
    let delimiter = List.hd !here_document_delimiters
    and skip_tabs = List.hd !here_document_skip_tabs
    and doc = Buffer.create 1000
    and nextline, pstart, pstop =
      match Prelexer.readline lexbuf with
        | None -> failwith "Unterminated here document."
        | Some (l, b, e) -> (ref l, ref b, ref e)
    in
    while (strip (if skip_tabs then untab !nextline else !nextline)
           <> delimiter)
    do
      Buffer.add_string doc !nextline;
      match Prelexer.readline lexbuf with
        | None -> failwith "Unterminated here document."
        | Some (l,b,e) -> nextline := l;
          pstop := e
    done;
    here_document_delimiters := List.tl !here_document_delimiters;
    here_document_skip_tabs := List.tl !here_document_skip_tabs;
    if !here_document_delimiters = [] then here_document_lexing := false;
    let before_stop = Lexing.({ !pstop with
      pos_cnum = !pstop.pos_cnum - 1;
      pos_bol  = !pstop.pos_bol  - 1;
    }) in
    push_pretoken (Prelexer.NEWLINE, before_stop, !pstop);
    fill_next_here_document_placeholder (CST.{
        value = Word (Buffer.contents doc);
        position = { start_p = CST.internalize !pstart;
                     end_p = CST.internalize!pstop }
    })
  in
  let rec next_token checkpoint =
    if !here_document_lexing then (
      next_here_document ();
      next_token checkpoint
    )
    else
      let (pretoken, pstart, pstop) as p = next_pretoken () in
      let return token =
        if token = EOF then eof := true;
        let token = if !eof then EOF else token in
        (token, pstart, pstop)
      in
      match pretoken with
        | Prelexer.IoNumber i ->
          return (IO_NUMBER (IONumber i))

        | Prelexer.Word w ->

        (**specification

           [Command Name]

           When the TOKEN is exactly a reserved word, the token
           identifier for that reserved word shall result. Otherwise,
           the token WORD shall be returned. Also, if the parser is in
           any state where only a reserved word could be the next
           correct token, proceed as above.

           Note: Because at this point <quotation-mark> characters are
           retained in the token, quoted strings cannot be recognized
           as reserved words. This rule also implies that reserved
           words are not recognized except in certain positions in the
           input, such as after a <newline> or <semicolon>; the
           grammar presumes that if the reserved word is intended, it
           is properly delimited by the user, and does not attempt to
           reflect that requirement directly. Also note that line
           joining is done before tokenization, as described in Escape
           Character (Backslash), so escaped <newline> characters are
           already removed at this point.  Rule 1 is not directly
           referenced in the grammar, but is referred to by other
           rules, or applies globally.

        *)
          let token = FirstSuccessMonad.(
            (recognize_assignment checkpoint p w)
            +> (recognize_reserved_word_if_relevant checkpoint p w)
            +> return (WORD (Word w))
          )
          in
          if !here_document_find_delimiter then (

            (**specification

                2.7.4 Here-Document

                If any part of word is quoted, the delimiter shall be
                formed by performing quote removal on word, and the
                here-document lines shall not be expanded. Otherwise,
                the delimiter shall be the word itself.
             *)
            here_document_delimiters :=
              (remove_quotes w) :: !here_document_delimiters;
            here_document_find_delimiter := false
          );
          return (FirstSuccessMonad.should_succeed token)

        | Prelexer.EOF ->
          real_eof := true;
          return EOF

        | Prelexer.Operator ((DLESS r | DLESSDASH r) as token) ->
          here_document_on_next_line := true;
          here_document_find_delimiter := true;
          here_document_placeholders := r :: !here_document_placeholders;
          let dashed = match token with DLESSDASH _ -> true | _ -> false in
          here_document_skip_tabs := dashed :: !here_document_skip_tabs;
          return token

        | Prelexer.Operator token ->
          return token

        | Prelexer.NEWLINE ->
        (** The interpretation of the pretoken [NEWLINE] depends
            on the parsing context: *)

        (** If we are to recognize a here-document, [NEWLINE] triggers
            the here-document lexing mode. *)
          if !here_document_on_next_line then (
            here_document_on_next_line := false;
            here_document_lexing := true;
            here_document_delimiters := List.rev !here_document_delimiters;
            here_document_skip_tabs := List.rev !here_document_skip_tabs;
            here_document_placeholders := List.rev !here_document_placeholders;
            next_token checkpoint
          )

        (** If the input is completed, [NEWLINE] is interpreted
            as the end-of-file marker. *)
          else if finished (offer checkpoint (EOF, pstart, pstop)) then
            return EOF

        (** If the input is not completed but [NEWLINE] as a meaning
            from the point of view of the grammar, it is promoted as a
            token and communicated to the parser. *)
          else if accepted_token checkpoint (NEWLINE, pstart, pstop) then
            return NEWLINE

        (** Otherwise, a [NEWLINE] is simply layout and is ignored. *)
          else next_token checkpoint
  in

    (**--------------**)
    (** Parsing loop. *)
    (**--------------**)

  let rec parse previous_state checkpoint =
    match checkpoint with
      (**

         If the parser requires some extra input to continue
         the analyze, [next_token] is called with the current
         parsing state as argument.

      *)
      | InputNeeded parsing_state ->
        let (token, ps, pe) as input = next_token checkpoint in
        parse (Some (input, checkpoint)) (offer checkpoint (token, ps, pe))

    (**

       If the parser has recognized a complete command and
       we are not at the end of the input, we restart a parser
       on the sequel.

    *)
      | Accepted cst ->
        eof := false;
        if !real_eof then
          [cst]
        else
          cst :: parse None (complete_command lexbuf.Lexing.lex_curr_p)

    (**

       The parser has rejected the input.

    *)
    (* FIXME: Generate a better error message. *)
      | Rejected ->
        raise ParseError

      (**

         The specification grammar has two incompleteness problems:
         some rules are missing to denote the usage of semicolons as
         <newline> separators and the start symbol should have an
         extra rule to accept an empty input.

         Indeed, semicolons are overloaded in the specification: they
         are legit tokens appearing in the grammar and they also are
         equivalent to newline characters at some specific
         points. Instead of polluting the official grammar to handle
         this second case, we use speculative parsing to try to
         replace a semicolon with a newline when a syntax error is
         raised.

         To deal with the second incompleteness of the grammar, we
         detect parsing errors that are raised when an empty input
         is provided to the parser. In that case, we simply accept
         the program.

         FIXME: Is that clear that we do not introduce more scripts in
         the language?

    *)

      | HandlingError env ->
        begin match previous_state with
        | Some ((Semicolon, ps, es), checkpoint) ->
          parse None (offer checkpoint (NEWLINE, ps, es))
        | Some ((EOF, _, _), _)
          when MenhirInterpreter.current_state_number env = 0 ->
          []
        | _ ->
          parse None (resume checkpoint)
        end

      (**

         The shell grammar follows a parsing-dependent lexical
         analysis: they are some places where a reserved word must be
         recognized as a simple word when it cannot be written at a
         given place of the input (see
         [recognize_reserved_word_if_relevant] defined
         earlier). However, they are some other places where this
         conversion from reserved words to simple words is forbidden.

         For instance, while the input

         `` echo else ``

         is syntactically correct, the input

         `` else echo ``

         is not.

         Instead of complicating
         [recognize_reserved_word_if_relevant], we decided to detect a
         posteriori when the conversion from reserved words to simple
         words should not have been made. This detection is easily
         feasible because there is actually only one place in the
         grammar where this conversion is forbidden: a reserved word
         can never be converted to a simple word where a [cmd_word] is
         expected.

         Fortunately, menhir gives us the control back when it is
         about to reduce a nonterminal. Therefore, it is possible to
         detect when a simple word, which is also a reserved word, has
         been reduced to a [cmd_word].

      *)
      | AboutToReduce (env, production) ->
        begin try
          if lhs production = X (N N_cmd_word)
          || lhs production = X (N N_cmd_name) then
            let analyse_top : type a. a symbol * a -> _ = function
              | T T_NAME, Name w when is_reserved_word w -> raise ParseError
              | T T_WORD, Word w when is_reserved_word w -> raise ParseError
              | _ -> raise Not_found
            in
            match top env with
            | Some (Element (state, v, _, _)) ->
              analyse_top (incoming_symbol state, v)
            | _ ->
              raise Not_found
          else raise Not_found
        with Not_found -> parse previous_state (resume checkpoint)
      end
    (**

       The other intermediate steps of the parser are ignored.

    *)

      | Shifting (_, _, _) ->
        parse previous_state (resume checkpoint)

  in
  parse None (complete_command lexbuf.Lexing.lex_curr_p)

let rec json_filter_positions =
  let open Yojson.Safe in
  function
  | `Assoc sjl ->
     if List.for_all (fun (s, j) -> s = "value" || s = "position") sjl then
       let (_, j) = List.find (fun (s, _) -> s = "value") sjl in
       json_filter_positions j
     else
       `Assoc (List.map (fun (s, j) -> Format.printf "%s@." s; (s, json_filter_positions j)) sjl)
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int i -> `Int i
  | `Intlit s -> `Intlit s
  | `List jl -> `List (List.map json_filter_positions jl)
  | `Null -> `Null
  | `String s -> `String s
  | `Tuple jl -> `Tuple (List.map json_filter_positions jl)
  | `Variant (s, None) -> `Variant (s, None)
  | `Variant (s, Some j) -> `Variant (s, Some (json_filter_positions j))

let save_as_json simplified cout csts =
  CST.complete_command_list_to_json csts
  |> (if simplified then json_filter_positions else function x-> x)
  |> Yojson.Safe.pretty_to_channel cout

let other_scripts_magic_strings =
  List.map Str.regexp [
             "#![ ]*/usr/bin/perl.*";
             "#![ ]*/bin/bash.*"
           ]

let is_other_script filename =
  (* check whether [filename] is a script other than /bin/sh *)
  let cin = open_in filename in
  let firstline = input_line cin in
  close_in cin;
  List.exists
    (function r -> Str.string_match r firstline 0)
    other_scripts_magic_strings

let is_elf filename =
  (* check whether [filename] is an ELF executable *)
  let cin = open_in_bin filename
  and buf = Bytes.create 4 in
  let number_chars_read = input cin buf 0 4 in
  begin
    close_in cin;
    if number_chars_read < 4
    then false
    else (Bytes.compare buf (Bytes.of_string  "\x7FELF")) = 0
  end

let parse_file filename =
  (** We assume that scripts are no longer than 16M. *)
  let cin = open_in filename in
  let cst =
    try parse (ExtPervasives.string_of_channel cin)
    with e -> close_in cin;
              raise e
  in
  close_in cin;
  cst
