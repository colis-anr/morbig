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

open API

let save input_filename (cst : CST.complete_command list) =
  (** write the concrete syntax tree [cst] to the output file corresponding
      to [input_filename]. The format and and the name of the output file
      are determined by the program options. *)
  let cout = open_out (Options.output_file_of_input_file input_filename) in
  Options.(begin match backend () with
           | Bin -> output_value cout (input_filename, cst)
           | Json -> JsonHelpers.save_as_json false cout cst
           | SimpleJson -> JsonHelpers.save_as_json true cout cst
           end);
  close_out cout
            
let save_error input_filename message =
  (** write string [message] to the error file corresponding to
      [input_filename]. *)
  let eout = open_out (input_filename ^ ".morbigerror") in
  output_string eout message;
  output_string eout "\n";
  close_out eout
                
let string_of_exn = function
  | Errors.ParseError pos ->
     Printf.sprintf "%s: Syntax error."
       CSTHelpers.(string_of_lexing_position pos)
  | Errors.LexicalError (pos, msg) ->
     Printf.sprintf "%s: Lexical error (%s)."
       CSTHelpers.(string_of_lexing_position pos)
       msg
  | Failure s ->
     "Failure: " ^ s ^ "."
  | Sys_error s ->
     "Error: " ^ s ^ "."
  | e -> raise e

let main =
  let parse_one_file input_filename =
    if Options.skip_nosh () &&
         (Scripts.is_elf input_filename ||
            Scripts.is_other_script input_filename)
    then begin
        Printf.eprintf "Skipping: %s.\n" input_filename;
      end
    else
      try
        parse_file input_filename |> save input_filename
      with e -> if Options.continue_after_error ()
                then
                  save_error input_filename (string_of_exn e)
                else (
                  output_string stderr (string_of_exn e ^ "\n");
                  exit 1
                )
  in
  Options.analyze_command_line_arguments ();
  if List.length (Options.input_files ()) <= 0 then (
    Printf.eprintf "morbig: no input files.\n";
    exit 1
  );
  List.iter parse_one_file (Options.input_files ())
