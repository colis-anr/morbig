(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017 Yann Régis-Gianas, Nicolas Jeannerod,             *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

(* Handling of command line options *)

(* different types of output *)
type backend = Json | Bin | SimpleJson

(* the output type chose on the command line *)
val backend : unit -> backend

(* the list of input files specified on the command line *)
val input_files : unit -> string list

(* return an ouput file name for a given input file name *)
val output_file_of_input_file : string -> string

(* tells whether input files which are ELF, or have a bash or perl magic *)
(* string, should be skipped. *)
val skip_nosh : unit -> bool

(* tells whether parsing shoudl continue with the next input file after *)
(* an error. *)
val continue_after_error : unit -> bool
                                            
(* parse the command line arguments *)
val analyze_command_line_arguments : unit -> unit
