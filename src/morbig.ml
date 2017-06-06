open API

let save filename (cst : CST.complete_command list) =
  let cout = open_out (Options.output_file_of_input_file filename) in
  Options.(begin match backend () with
  | Bin -> output_value cout (filename, cst)
  | Json -> Engine.save_as_json cout cst
  end);
  close_out cout

let main =
  Options.analyze_command_line_arguments ();
  if List.length (Options.input_files ()) <= 0 then (
    Printf.eprintf "morbig: no input files.\n";
    exit 1
  );
  List.iter (function input ->
      if Options.skip_nosh () &&
         Engine.is_elf input || Engine.is_other_script input
      then begin
        Printf.eprintf "Skipping: %s.\n" input;
        exit 0
      end
      else parse_file input |> save input
    ) (Options.input_files ())
