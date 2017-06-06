open API

let main =
  Options.analyze_command_line_arguments ();
  if List.length (Options.input_files ()) <= 0 then (
    Printf.eprintf "morbig: no input files.\n";
    exit 1
  );
  let input = Sys.argv.(1) in
  if Engine.is_elf input || Engine.is_other_script input
  then begin
      Printf.eprintf "Skipping: %s.\n" input;
      exit 0
    end
  else parse_file input |> Engine.save input
