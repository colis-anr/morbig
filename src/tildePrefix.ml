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

(*specification

  A "tilde-prefix" consists of an unquoted <tilde> character at the
   beginning of a word, followed by all of the characters preceding
   the first unquoted <slash> in the word, or all the characters in
   the word if there is no <slash>. In an assignment (see XBD Variable
   Assignment), multiple tilde-prefixes can be used: at the beginning
   of the word (that is, following the <equals-sign> of the
   assignment), following any unquoted <colon>, or both. A
   tilde-prefix in an assignment is terminated by the first unquoted
   <colon> or <slash>. If none of the characters in the tilde-prefix
   are quoted, the characters in the tilde-prefix following the
   <tilde> are treated as a possible login name from the user
   database. A portable login name cannot contain characters outside
   the set given in the description of the LOGNAME environment
   variable in XBD Other Environment Variables. If the login name is
   null (that is, the tilde-prefix contains only the tilde), the
   tilde-prefix is replaced by the value of the variable HOME. If HOME
   is unset, the results are unspecified. Otherwise, the tilde-prefix
   shall be replaced by a pathname of the initial working directory
   associated with the login name obtained using the getpwnam()
   function as defined in the System Interfaces volume of
   POSIX.1-2017. If the system does not recognize the login name, the
   results are undefined.

*)
open CST

(** Tests whether the given string starts with a tilde character. *)
let starts_with_tilde string =
  string != "" && string.[0] = '~'

(** Removes the ['~'] character at the beginning of the given string or fail
    with [Invalid_arg] if the string does not start with a tilde. *)
let strip_tilde string =
  if not (starts_with_tilde string) then
    invalid_arg "strip_tilde";
  String.(sub string 1 (length string - 1))

(** Extracts the tilde-prefix at the beginning of the given literal string or
    fail with [Invalid_arg] if it does not start with a tilde. *)
let extract_tilde_prefix_from_literal (literal : string) : word_cst =
  if not (starts_with_tilde literal) then
    invalid_arg "extract_tilde_prefix_from_literal";
  match String.split_on_char '/' literal with
  | [] ->
    (* [String.split_on_char] yields a list of at least one element. *)
    assert false
  | [first] ->
    [
      WordTildePrefix (strip_tilde first);
    ]
  | first :: rest ->
    [
      WordTildePrefix (strip_tilde first);
      WordLiteral ("/" ^ String.concat "/" rest);
    ]

(** Merges several leading [WordLiteral] into one. *)
let rec merge_leading_literals : word_cst -> word_cst = function
  | WordLiteral literal1 :: WordLiteral literal2 :: word ->
    merge_leading_literals (WordLiteral (literal1 ^ literal2) :: word)
  | word -> word

(** Extracts the tilde-prefix at the beginning of the given word CST if there is
    one. Otherwise, returns the word as-is. *)
let extract_tilde_prefix_from_word_if_present (word : word_cst) : word_cst =
  match merge_leading_literals word with
  | WordLiteral literal :: word when starts_with_tilde literal ->
    extract_tilde_prefix_from_literal literal @ word
  | word -> word

(** Extracts the first and last elements of a list; returns a triple consisting
    of the first element, the “body” of the list and the last element. Assumes
    that the list has at least two elements or raises [Invalid_arg]. *)
let extract_list_head_and_foot (list : 'a list) : 'a * 'a list * 'a =
  let rec extract_list_foot (list : 'a list) : 'a list * 'a =
    match list with
    | [] -> assert false
    | [foot] -> ([], foot)
    | first :: rest ->
      let (body, foot) = extract_list_foot rest in
      (first :: body, foot)
  in
  match list with
  | [] | [_] -> invalid_arg "extract_list_head_and_foot"
  | head :: rest ->
    let (body, foot) = extract_list_foot rest in
    (head, body, foot)

(** Splits the given word on each literal colon character, returning a list of
    words. A word semantically equivalent can be re-obtained by interspersing a
    [WordLiteral ":"] between all the words. *)
(* REVIEW: this might make sense on its own as one of the CST helpers. *)
let split_word_on_colon (word_to_process : word_cst) : word_cst list =
  let rec split_word_component_on_colon
      ~(processed_words_rev : word_cst list)
      ~(current_word_rev : word_cst)
      ~(word_to_process : word_cst)
    : word_cst list
    =
    match word_to_process with
    | WordLiteral literal :: word_to_process when String.contains literal ':' ->
      let subliterals = String.split_on_char ':' literal in
      let (first, body, foot) = extract_list_head_and_foot subliterals in
      let processed_words_rev =
        List.rev_map (fun literal -> [WordLiteral literal]) body
        @ [List.rev (WordLiteral first :: current_word_rev)]
        @ processed_words_rev
      in
      split_word_component_on_colon
        ~processed_words_rev
        ~current_word_rev:[WordLiteral foot]
        ~word_to_process

    | word_component :: word_to_process ->
      split_word_component_on_colon
        ~processed_words_rev
        ~current_word_rev:(word_component :: current_word_rev)
        ~word_to_process

    | [] ->
      List.rev ((List.rev current_word_rev) :: processed_words_rev)
  in
  split_word_component_on_colon
    ~processed_words_rev:[]
    ~current_word_rev:[]
    ~word_to_process

(** Concatenates the given list of words into one, interspersing it with literal
    colon characters. *)
let rec concat_words_with_colon (words : word_cst list) : word_cst =
  match words with
  | [] -> []
  | [word] -> word
  | word :: words -> word @ [WordLiteral ":"] @ concat_words_with_colon words

(** Recognises tilde prefixes in a word. The [rhs_assignment] parameter
    influences the behaviour of the recognition as described by the standard. *)
let recognize (word : word_cst) =
  match word with
  | [WordAssignmentWord (name, Word (s, word))] ->
    let words = split_word_on_colon word in
    let words = List.map extract_tilde_prefix_from_word_if_present words in
    let word = concat_words_with_colon words in
    [WordAssignmentWord (name, Word (s, word))]
  | _ ->
    extract_tilde_prefix_from_word_if_present word
