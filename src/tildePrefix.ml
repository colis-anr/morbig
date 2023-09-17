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

open ExtPervasives

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
  match String.index_opt literal '/' with
  | None -> [WordTildePrefix (strip_tilde literal)]
  | Some i ->
    let (first, rest) = ExtPervasives.string_split i literal in
    [WordTildePrefix (strip_tilde first); WordLiteral rest]

(** Extracts the tilde-prefix at the beginning of the given word CST if there is
    one. Otherwise, returns the word as-is. *)
let extract_tilde_prefix_from_word_if_present (word : word_cst) : word_cst =
  match CSTHelpers.merge_leading_literals word with
  | WordLiteral literal :: word when starts_with_tilde literal ->
    extract_tilde_prefix_from_literal literal @ word
  | word -> word

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
      (* Because [literal] contains [':'], then [subliterals] is guaranteed to
         be of size at least 2, and therefore [List.hd], [List.cr], and
         [List.tl] will not fail. *)
      let subliterals = String.split_on_char ':' literal in
      let processed_words_rev =
        List.rev_map (fun literal -> [WordLiteral literal]) (List.cr subliterals)
        @ [List.rev (WordLiteral (List.hd subliterals) :: current_word_rev)]
        @ processed_words_rev
      in
      split_word_component_on_colon
        ~processed_words_rev
        ~current_word_rev:[WordLiteral (List.ft subliterals)]
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

(** Recognises tilde prefixes in a word, that is recognises eg. [WordLiteral
    "~foo"] and replaces it by [WordTildePrefix "foo"] when in the right
    position. *)
let recognize ~in_assignment (word : word_cst) =
  if in_assignment then
    let words = split_word_on_colon word in
    let words = List.map extract_tilde_prefix_from_word_if_present words in
    concat_words_with_colon words
  else
    extract_tilde_prefix_from_word_if_present word
