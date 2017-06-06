(* Handling of command line options *)

(* different types of output *)
type backend = Json | Bin

(* the output type chose on the command line *)
val backend : unit -> backend

(* the list of input files specified on the command line *)
val input_files : unit -> string list

(* return an ouput file name for a given input file name *)
val output_file_of_input_file : string -> string

(* parse the command line arguments *)
val analyze_command_line_arguments : unit -> unit
