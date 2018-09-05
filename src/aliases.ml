(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
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

open ExtPervasives
open ExtMenhirLib
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open MenhirLib.General
open CST

(**

   A shell script may define aliases with the following command:

   ``` alias x='foo bar' ```

   Alias substitution are specified in the standard as follows:

*)

(*specification

   After a token has been delimited, but before applying the
   grammatical rules in Shell Grammar, a resulting word that is
   identified to be the command name word of a simple command shall be
   examined to determine whether it is an unquoted, valid alias
   name. However, reserved words in correct grammatical context shall
   not be candidates for alias substitution. A valid alias name (see
   XBD Alias Name) shall be one that has been defined by the alias
   utility and not subsequently undefined using
   unalias. Implementations also may provide predefined valid aliases
   that are in effect when the shell is invoked. To prevent infinite
   loops in recursive aliasing, if the shell is not currently
   processing an alias of the same name, the word shall be replaced by
   the value of the alias; otherwise, it shall not be replaced.

*)

open CST

type state =
  | NoRecentSubstitution
  | CommandNameSubstituted
  | NextWordSubstituted

type t = {
    state       : state;
    definitions : (string * string) list
  }

let empty = {
    state       = NoRecentSubstitution;
    definitions = []
  }

(** [bind_aliases to_bind aliases] returns an alias table obtained from
    [aliases] by adding all entries from [to_bind]. *)
let bind_aliases to_bind aliases =
  { aliases with definitions = to_bind @ aliases.definitions }

(** [unbind_aliases to_unbind aliases] returns an alias table obtained from
    [aliases] by omitting all entries from [to_unbind]. *)
let unbind_aliases to_unbind aliases =
  { aliases with
    definitions =
      List.filter (fun (x, _) -> not (List.mem x to_unbind)) aliases.definitions
  }

exception NestedAliasingCommand

type alias_related_command =
  | Alias of (string * string) list
  | Unalias of string list
  | Reset

let binder_from_alias (x:CST.cmd_suffix) =
  let open CSTHelpers in
  let open Str in
  let open List in
  let wl = wordlist_of_cmd_suffix x in
  fold_right (fun a accu ->
       let s = bounded_split (regexp "=") (on_located unWord a) 2 in
       if List.length s < 2 then
         accu
       else
         (hd s, hd (tl s)):: accu)
    wl
    []

let unalias_argument (x:CST.cmd_suffix) = CSTHelpers.(
  List.map (on_located unWord) (wordlist_of_cmd_suffix x)
)

let rec as_aliasing_related_command = function
  | SimpleCommand_CmdName_CmdSuffix ({ value = CmdName_Word w }, suffix) ->
    begin match w.value with
    | Word ("alias", _) ->
      let l = binder_from_alias suffix.value in
      Some (Alias l)
    | Word ("unalias", _) ->
      let l = unalias_argument suffix.value in
      Some (if l = ["-a"] then Reset else Unalias l)
    | _ ->
      None
    end
  | SimpleCommand_CmdName _
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix _
  | SimpleCommand_CmdPrefix_CmdWord _
  | SimpleCommand_CmdPrefix _ ->
    None

(** [interpret aliases cst] traverses [cst] to check that there are no
    alias or unalias invocations in a nested command, in which case an
    error is issued. Then, for any alias and unalias toplevel invocation,
    this function updates [aliases]. *)
let interpret aliases cst =
  let aliases = ref aliases in
  let level = ref 0 in
  let at_toplevel () = !level = 0 in
  let analyzer = object (self : 'self)
      inherit [_] CST.iter as super
      method! visit_compound_command env cmd =
        incr level;
        super # visit_compound_command env cmd;
        decr level

      method! visit_simple_command _ cmd =
        match as_aliasing_related_command cmd with
        | Some alias_command ->
          if at_toplevel () then match alias_command with
            | Alias x -> aliases := bind_aliases x !aliases
            | Unalias x -> aliases := unbind_aliases x !aliases
            | Reset -> aliases := empty
          else
            raise NestedAliasingCommand
        | None ->
          ()
    end
  in
  analyzer#visit_complete_command () cst;
  !aliases

let substitute aliases w =
  try
    List.assoc w aliases.definitions
  with Not_found ->
    w

(** [about_to_reduce_cmd_name checkpoint] *)
let rec about_to_reduce_cmd_name checkpoint =
  match checkpoint with
  | AboutToReduce (_, production) ->
     if lhs production = X (N N_linebreak) || lhs production = X (N N_word) then
       about_to_reduce_cmd_name (resume checkpoint)
     else
        lhs production = X (N N_cmd_name)
  | InputNeeded _ ->
    let dummy = Lexing.dummy_pos in
    let token = NAME (Name "a_word"), dummy, dummy in
    about_to_reduce_cmd_name (offer checkpoint token)
  | Shifting _ ->
    about_to_reduce_cmd_name (resume checkpoint)
  | _ ->
    false

(** [about_to_reduce_word checkpoint] *)
let rec about_to_reduce_word checkpoint =
  match checkpoint with
  | AboutToReduce (_, production) ->
    if lhs production = X (N N_linebreak) then
      about_to_reduce_word (resume checkpoint)
    else
      lhs production = X (N N_word)
  | InputNeeded _ ->
    let dummy = Lexing.dummy_pos in
    let token = NAME (Name "a_word"), dummy, dummy in
    about_to_reduce_word (offer checkpoint token)
  | Shifting _ ->
    about_to_reduce_word (resume checkpoint)
  | _ ->
    false

(** [inside_a_substitution_combo state] is true if a sequence of alias
   substitution is triggered by the following cornercase rule of the
   standard.*)
(*specification
 If the value of the alias replacing the word ends in a <blank>, the
 shell shall check the next command word for alias substitution; this
 process shall continue until a word is found that is not a valid alias
 or an alias value does not end in a <blank>.
*)
let inside_a_substitution_combo = function
  | CommandNameSubstituted | NextWordSubstituted -> true
  | _ -> false

let quoted word =
  let len = String.length word in
  len >= 2 && word.[0] = '\'' && word.[len - 1] = '\''

let unquote word =
  String.(sub word 1 (length word - 2))

let rec end_of_with_whitespace word =
  if quoted word then
    end_of_with_whitespace (unquote word)
  else
    let len = String.length word - 1 in
    len >= 1 && word.[String.length word - 1] = ' '

let only_if_end_with_whitespace word aliases state =
  if end_of_with_whitespace word then (
    ({ aliases with state }, word)
  ) else
    ({ aliases with state = NoRecentSubstitution }, word)

(** [alias_substitution aliases checkpoint word] substitutes an
    alias by its definition if word is not a reserved word and
    if the parsing context is about to reduce a [cmd_name]. *)
let alias_substitution aliases checkpoint word =
  if about_to_reduce_cmd_name checkpoint
     && not (Keyword.is_reserved_word word)
  then
    let word = substitute aliases word in
    only_if_end_with_whitespace word aliases CommandNameSubstituted
  else
    if about_to_reduce_word checkpoint
       && inside_a_substitution_combo aliases.state
    then
      let word' = substitute aliases word in
      only_if_end_with_whitespace word' aliases NextWordSubstituted
    else
      (aliases, word)
