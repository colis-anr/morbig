type backend =
  | Json
  | Bin

let _backend = ref Json
let backend_of_string = function
  | "json" -> Json
  | "bin" -> Bin
  | _ -> assert false
let set_backend x = x |> backend_of_string |> (( := ) _backend)
let backend () = !_backend

let _input_files = ref []
let append_file f = _input_files := f :: !_input_files
let input_files () = !_input_files

let _output_file = ref "morbig.out"
let output_file () = !_output_file

let usage_msg = "\
Usage: morbig [options] file...
"

let analyze_command_line_arguments () = Arg.(
    let options = [
      "--as", Symbol ([ "json"; "bin" ], set_backend),
      " Set the output format. (default is json.)";

      "--output", Set_string _output_file,
      " Set the output filename. (default is morbig.out.)"
    ]
    in
    parse (align options) append_file usage_msg
  )
