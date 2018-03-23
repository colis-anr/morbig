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

let complete_command_to_json c =
  complete_command_to_yojson c

let complete_command_list_to_json cl =
  complete_command_list_to_yojson cl

let unWord (Word (s, _)) = s

let unName (Name s) = s

let word_of_name (Name w) = Word (w, WordName)

let word_of_assignment_word (AssignmentWord (Name n, (Word (s, k) as w))) =
  Word (n ^ "=" ^ s, WordAssignmentWord w)

let string_of_word (Word (s, _)) = s

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let word_placeholder () =
  let dummy_lexing_position = {
      pos_fname = "";
      pos_lnum  = -1;
      pos_bol   = -1;
      pos_cnum  = -1;
    }
  in let dummy_position = {
         start_p = dummy_lexing_position;
         end_p = dummy_lexing_position;
       }
     in
     ref {
         value = Word ("<you should not see this>", WordOther);
         position = dummy_position
       }

let internalize p = {
  pos_fname = p.Lexing.pos_fname;
  pos_lnum  = p.Lexing.pos_lnum;
  pos_bol   = p.Lexing.pos_bol;
  pos_cnum  = p.Lexing.pos_cnum;
}

let externalize p = {
  Lexing.pos_fname = p.pos_fname;
  pos_lnum  = p.pos_lnum;
  pos_bol   = p.pos_bol;
  pos_cnum  = p.pos_cnum;
}

let with_poss p1 p2 v =
  with_pos { start_p = internalize p1; end_p = internalize p2 } v

module NameSet = Set.Make (struct
  type t = name
  let compare (Name s1) (Name s2) = String.compare s1 s2
end)

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

let nonempty_complete_command = function
  | CompleteCommand_Empty -> false
  | _ -> true


(** [wordlist_of_cmd_suffix] extracts the list of all words from a cmd_sufix *)
let rec wordlist_of_cmd_suffix = function
  | CmdSuffix_IoRedirect _io_redirect' ->
     []
  | CmdSuffix_CmdSuffix_IoRedirect (cmd_suffix',_io_redirect') ->
     wordlist_of_cmd_suffix cmd_suffix'.value
  | CmdSuffix_Word word'->
     [word'.value]
  | CmdSuffix_CmdSuffix_Word (cmd_suffix',word') ->
     (wordlist_of_cmd_suffix cmd_suffix'.value)@[word'.value]
