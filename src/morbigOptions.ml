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

type backend =
  | Json
  | Bin
  | SimpleJson
  | Dot

let _backend = ref Json
let backend_of_string = function
  | "json" -> Json
  | "bin" -> Bin
  | "simple" -> SimpleJson
  | "dot" -> Dot
  | _ -> assert false
let set_backend x = x |> backend_of_string |> (( := ) _backend)
let backend () = !_backend

let _input_files = ref []
let append_file f = _input_files := f :: !_input_files
let input_files () = !_input_files

let output_file_of_input_file s =
  match backend () with
  | Json -> s ^ ".json"
  | Bin -> s ^ ".morbig"
  | SimpleJson -> s ^ ".sjson"
  | Dot -> s ^ ".dot"

let _skip_nosh = ref false
let skip_nosh () = !_skip_nosh

let _continue_after_error = ref false
let continue_after_error () = !_continue_after_error

let _display_stats = ref false
let display_stats () = !_display_stats

let _from_stdin = ref false
let from_stdin () = !_from_stdin

let _debug = ref false
let debug () = !_debug

let usage_msg = "\
Usage: morbig [options] file...
"

let show_version_and_exit () =
  Printf.printf "morbig %s (compiled from commit %s, committed on %s)\n"
    Version.current Version.commithash Version.commitdate;
  exit 0

let analyze_command_line_arguments () = Arg.(
    let options = [
      "--as", Symbol ([ "json"; "bin"; "simple"; "dot" ], set_backend),
      " Set the output format. (default is json.)";

      "--skip-nosh", Set _skip_nosh,
      " Skip input files that are ELF, or have a bash or perl magic string";

      "--continue-after-error", Set _continue_after_error,
      " Continue after error with the next script";

      "--display-stats", Set _display_stats,
      " Display statistics on failures and skipped files";

      "--from-stdin", Set _from_stdin,
      " Get names of scripts to parse from standard input.";

      "--debug", Set _debug,
      " Activate debugging information.";

      "--version", Unit show_version_and_exit,
      " Show version number and exit."
    ]
    in
    parse (align options) append_file usage_msg
  )
