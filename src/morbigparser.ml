let save_as_json cout csts =
  CST.complete_command_list_to_json csts |>
  Yojson.Safe.to_channel cout

let save filename (cst : CST.complete_command list) =
  let cout = open_out (Options.output_file ()) in
  Options.(begin match backend () with
  | Bin -> output_value cout (filename, cst)
  | Json -> save_as_json cout cst
  end);
  close_out cout

let other_scripts_magic_strings =
  List.map Str.regexp [
             "#![ ]*/usr/bin/perl.*";
             "#![ ]*/bin/bash.*"
           ]

let is_other_script filename =
  (* check whether [filename] is a script other than /bin/sh *)
  let cin = open_in filename in
  let firstline = input_line cin in
  close_in cin;
  List.exists
    (function r -> Str.string_match r firstline 0)
    other_scripts_magic_strings

let is_elf filename =
  (* check whether [filename] is an ELF executable *)
  let cin = open_in_bin filename
  and buf = Bytes.create 4 in
  let number_chars_read = input cin buf 0 4 in
  begin
    close_in cin;
    if number_chars_read < 4
    then false
    else (Bytes.compare buf (Bytes.of_string  "\x7FELF")) = 0
  end

let parse_file filename =
  (** We assume that scripts are no longer than 16M. *)
  let cin = open_in filename in
  let cst = Morbig.parse (ExtPervasives.string_of_channel cin) in
  close_in cin;
  cst

let main () =
  Options.analyze_command_line_arguments ();
  Morbig.main ();
  if List.length (Options.input_files ()) <= 1 then (
    Printf.eprintf "morbig: no input files.\n";
    exit 1
  );
  let input = Sys.argv.(1) in
  if is_elf input || is_other_script input
  then begin
      Printf.eprintf "Skipping: %s.\n" input;
      exit 0
    end
  else parse_file input |> save input
;;

  main()
;;
