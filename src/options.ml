type backend =
  | Json
  | Bin
  | SimpleJson

let _backend = ref Json
let backend_of_string = function
  | "json" -> Json
  | "bin" -> Bin
  | "simple" -> SimpleJson
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

let _skip_nosh = ref false
let skip_nosh () = !_skip_nosh

let _continue_after_error = ref false
let continue_after_error () = !_continue_after_error

let usage_msg = "\
Usage: morbig [options] file...
"

let analyze_command_line_arguments () = Arg.(
    let options = [
      "--as", Symbol ([ "json"; "bin"; "simple" ], set_backend),
      "Set the output format. (default is json.)";

      "--skip-nosh", Set _skip_nosh,
      "Skip input files that are ELF, or have a bash or perl magic string";

      "--continue-after-error", Set _continue_after_error,
      "Continue after error with the next script";
    ]
    in
    parse (align options) append_file usage_msg
  )
