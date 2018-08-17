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

open CST

(* Helpers about positions *)

let on_located f x = f x.value

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let dummy_lexing_position = {
    pos_fname = "";
    pos_lnum  = -1;
    pos_bol   = -1;
    pos_cnum  = -1;
  }

let dummy_position = {
    start_p = dummy_lexing_position;
    end_p = dummy_lexing_position;
  }

let with_poss p1 p2 v =
  with_pos { start_p = p1; end_p = p2 } v

let start_of_position p = p.start_p

let end_of_position p = p.end_p

let filename_of_position p =
  p.start_p.pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let emacs_position filename linenumber cs =
  Printf.sprintf "%sine %d%s"
    (if filename = "" then
       "L"
     else
       Printf.sprintf "File \"%s\", l" filename
    )
    linenumber
    (match cs with
     | [] -> ""
     | [c] -> Printf.sprintf ", character %d" c
     | c1 :: c2 :: _ -> Printf.sprintf ", characters %d-%d" c1 c2)

let string_of_lexing_position p =
  emacs_position p.pos_fname (line p) [column p]

let string_of_position p =
  let filename = filename_of_position p in
  let l = line p.start_p in
  let c1, c2 = characters p.start_p p.end_p in
    if filename = "" then
      Printf.sprintf "Line %d, characters %d-%d" l c1 c2
    else
      Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let compare_positions p1 p2 =
  compare p1.start_p.pos_cnum p2.start_p.pos_cnum

let merge_positions p1 p2 =
  { start_p = p1.start_p; end_p = p2.end_p }

(* Helpers about complete commands *)

let nonempty_program = function
  | Program_LineBreak _ -> false
  | _ -> true

let empty_linebreak' =
  with_pos dummy_position LineBreak_Empty

let empty_program =
  Program_LineBreak empty_linebreak'

let nonempty_program p =
  match p with
  | Program_LineBreak _ ->
     false
  | _ ->
     true

let rec concat_complete_commands' cs1 cs2 =
  let pos = merge_positions cs1.position cs2.position in
  with_pos pos @@
    match cs2.value with
    | CompleteCommands_CompleteCommand c2 ->
       let nl = with_pos cs1.position NewLineList_NewLine in
       CompleteCommands_CompleteCommands_NewlineList_CompleteCommand (
           cs1,
           nl,
           c2)
    | CompleteCommands_CompleteCommands_NewlineList_CompleteCommand (cs2, nl, c2) ->
       CompleteCommands_CompleteCommands_NewlineList_CompleteCommand (
           concat_complete_commands' cs1 cs2,
           nl,
           c2
         )

let concat_programs p1 p2 =
  let pos = merge_positions p1.position p2.position in
  with_pos pos @@
    match p1.value, p2.value with
    | Program_LineBreak _, p | p, Program_LineBreak _ ->
       p
    | Program_LineBreak_CompleteCommands_LineBreak (pnl1, cs1, snl1),
      Program_LineBreak_CompleteCommands_LineBreak (pnl2, cs2, snl2) ->
       Program_LineBreak_CompleteCommands_LineBreak (
           pnl1,
           concat_complete_commands' cs1 cs2,
           snl2
         )

let program_to_json p =
  program_to_yojson p

let complete_command_to_json c =
  complete_command_to_yojson c

let program_to_json cl =
  program_to_yojson cl

(* Helpers about words and names *)

let unWord (Word (s, _)) = s

let unName (Name s) = s

let word_of_name (Name w) = Word (w, [WordName w])

let word_of_assignment_word (Name n, (Word (s, _) as w)) =
  Word (n ^ "=" ^ s, [WordAssignmentWord (Name n, w)])

let string_of_word (Word (s, _)) = s

let word_placeholder () =
  ref {
      value = Word ("<you should not see this>", []);
      position = dummy_position
    }

module NameSet = Set.Make (struct
  type t = name
  let compare (Name s1) (Name s2) = String.compare s1 s2
end)

(* CST destructors *)

(** [wordlist_of_cmd_suffix] extracts the list of all words from a cmd_sufix *)
let rec wordlist_of_cmd_suffix = function
  | CmdSuffix_IoRedirect _io_redirect' ->
     []
  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix',_io_redirect') ->
     wordlist_of_cmd_suffix cmd_suffix'.value
  | CmdSuffix_Word word'->
     [word']
  | CmdSuffix_CmdSuffix_Word (cmd_suffix',word') ->
     (wordlist_of_cmd_suffix cmd_suffix'.value) @ [word']

let io_redirect_list_of_cmd_prefix cmd_prefix =
  let rec aux acc = function
    | CmdPrefix_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | CmdPrefix_CmdPrefix_IoRedirect (cmd_prefix', io_redirect') ->
       aux (io_redirect' :: acc) cmd_prefix'.value
    | CmdPrefix_AssignmentWord _ ->
       acc
    | CmdPrefix_CmdPrefix_AssignmentWord (cmd_prefix', _) ->
       aux acc cmd_prefix'.value
  in
  aux [] cmd_prefix

let io_redirect_list_of_cmd_suffix cmd_suffix =
  let rec aux acc = function
    | CmdSuffix_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix', io_redirect') ->
       aux (io_redirect' :: acc) cmd_suffix'.value
    | CmdSuffix_Word _ ->
       acc
    | CmdSuffix_CmdSuffix_Word (cmd_suffix', _) ->
       aux acc cmd_suffix'.value
  in
  aux [] cmd_suffix

let io_redirect_list_of_redirect_list redirect_list =
  let rec aux acc = function
    | RedirectList_IoRedirect io_redirect' ->
       io_redirect' :: acc
    | RedirectList_RedirectList_IoRedirect (redirect_list', io_redirect') ->
       aux (io_redirect' :: acc) redirect_list'.value
  in
  aux [] redirect_list

let io_redirect_list_of_simple_command = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cmd_prefix', _, cmd_suffix') ->
     (io_redirect_list_of_cmd_prefix cmd_prefix'.value)
     @ (io_redirect_list_of_cmd_suffix cmd_suffix'.value)
  | SimpleCommand_CmdPrefix_CmdWord (cmd_prefix', _)
  | SimpleCommand_CmdPrefix cmd_prefix' ->
     io_redirect_list_of_cmd_prefix cmd_prefix'.value
  | SimpleCommand_CmdName_CmdSuffix (_, cmd_suffix') ->
     io_redirect_list_of_cmd_suffix cmd_suffix'.value
  | SimpleCommand_CmdName _ ->
     []
