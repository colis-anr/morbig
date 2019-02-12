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

open Morbig
open API

let save input_filename (cst : CST.program) =
  MorbigOptions.(
    let cout = open_out (output_file_of_input_file input_filename) in
    begin match backend () with
    | Bin -> API.save_binary_cst cout cst
    | Json -> API.save_json_cst cout cst
    | SimpleJson -> JsonHelpers.save_as_json true cout cst
    | Dot -> JsonHelpers.save_as_dot cout cst
    end;
    close_out cout
  )
(** write the concrete syntax tree [cst] to the output file
   corresponding to [input_filename]. The format and the name of the
   output file are determined by the program options. *)

let save_error input_filename message =
  let eout = open_out (input_filename ^ ".morbigerror") in
  output_string eout message;
  output_string eout "\n";
  close_out eout
(** write string [message] to the error file corresponding to
   [input_filename]. *)

let not_a_script input_filename =
  MorbigOptions.skip_nosh ()
  && (Scripts.(is_elf input_filename || is_other_script input_filename))

let nb_inputs = ref 0
let nb_inputs_skipped = ref 0
let nb_inputs_erroneous = ref 0

let show_stats () =
  if MorbigOptions.display_stats () then begin
      Printf.printf "Number of input files: %i\n" !nb_inputs;
      Printf.printf "Number of skipped files: %i\n" !nb_inputs_skipped;
      Printf.printf "Number of rejected files: %i\n" !nb_inputs_erroneous
    end

let parse_one_file input_filename =
  Debug.printf "Trying to open: %s\n" input_filename;
  incr nb_inputs;
  if not_a_script input_filename then
    incr nb_inputs_skipped
  else
    try
      parse_file input_filename |> save input_filename
    with e ->
      incr nb_inputs_erroneous;
      if MorbigOptions.continue_after_error () then
        save_error input_filename (Errors.string_of_error e)
      else (
        output_string stderr (Errors.string_of_error e ^ "\n");
        exit 1
      )

let parse_input_files_provided_via_stdin () =
  try
    while true do
      parse_one_file (read_line ())
    done
  with End_of_file -> ()

let parse_input_files_provided_on_command_line () =
  if List.length (MorbigOptions.input_files ()) <= 0 then begin
      Printf.eprintf "morbig: no input files.\n";
      exit 1
    end;
  List.iter parse_one_file (MorbigOptions.input_files ())

let parse_input_files () =
  if MorbigOptions.from_stdin () then
    parse_input_files_provided_via_stdin ()
  else
    parse_input_files_provided_on_command_line ()

let main =
  MorbigOptions.analyze_command_line_arguments ();
  parse_input_files ();
  show_stats ()
