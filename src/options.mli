val analyze_command_line_arguments : unit -> unit

val input_files : unit -> string list

val output_file : unit -> string

type backend =
  | Json
  | Bin

val backend : unit -> backend
