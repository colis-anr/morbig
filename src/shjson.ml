open Shparser (* FIXME: this parses everything in the command line and save it in marshal. This is soooo dirty. *)

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let filename = Sys.argv.(i) in

    (* Extracts a Marshal dump and produces a JSon dump *)
    
    let dumpname = filename ^ ".binsh" in
    let dump_channel = open_in dumpname in
    let (_, csts) : string * CST.complete_command list = input_value dump_channel in
    close_in dump_channel;
    
    let dumpname = filename ^ ".json" in
    let dump_channel = open_out dumpname in
    let dump_formatter = Format.formatter_of_out_channel dump_channel in
    
    CST.complete_command_list_to_json csts
    |> Format.fprintf dump_formatter "%s@.";
    
    close_out dump_channel
  done
