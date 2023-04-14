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

open CST

(** {2 Helpers about programs and complete commands} *)

val empty_program : program located
val nonempty_program : program -> bool
val concat_programs : program located -> program located -> program located

(** {2 Helpers about words and names} *)

val unWord : word -> string
val unName : name -> string

val word_of_name : name -> word

val word_of_assignment_word : assignment_word -> word

val string_of_word : word -> string

val word_placeholder : unit -> word' ref

exception InvalidFunctionName

val make_function_name: name -> fname

module NameSet : Set.S

(** {2 Helpers about positions} *)

val on_located : ('a -> 'b) -> 'a located -> 'b

val with_pos : position -> 'a -> 'a located
val with_poss : Lexing.position -> Lexing.position -> 'a -> 'a located

val dummy_lexing_position : Lexing.position
val dummy_position : position

val start_of_position : position -> Lexing.position
val end_of_position : position -> Lexing.position
val filename_of_position : position -> string

val line : Lexing.position -> int
val column : Lexing.position -> int

val characters : Lexing.position -> Lexing.position -> int * int

val emacs_position : string -> int -> int list -> string

val string_of_lexing_position : Lexing.position -> string
val string_of_position : position -> string

val compare_positions : position -> position -> int

(** {2 CST destructors} *)

(** [wordlist_of_cmd_suffix] extracts the list of all words from a cmd_sufix *)
val wordlist_of_cmd_suffix : cmd_suffix -> word' list

val io_redirect_list_of_cmd_prefix : cmd_prefix -> io_redirect' list
val io_redirect_list_of_cmd_suffix : cmd_suffix -> io_redirect' list
val io_redirect_list_of_simple_command : simple_command -> io_redirect' list
val io_redirect_list_of_redirect_list : redirect_list -> io_redirect' list
