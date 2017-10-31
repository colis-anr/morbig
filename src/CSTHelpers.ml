open CST

let complete_command_to_json c =
  complete_command_to_yojson c

let complete_command_list_to_json cl =
  complete_command_list_to_yojson cl

let unWord (Word s) = s

let unName (Name s) = s

let word_of_name (Name w) = Word w

let word_of_assignment_word (AssignmentWord (Name n, Word s)) =
  Word (n ^ "=" ^ s)

let string_of_word (Word s) = s

let with_pos p v =
  {
    value     = v;
    position  = p;
  }

let dummy_lexing_position = {
  pos_fname = "";
  pos_lnum  = -1;
  pos_bol   = -1;
  pos_cnum  = -1;
}

let dummy_position = {
  start_p = dummy_lexing_position;
  end_p = dummy_lexing_position;
}

let internalize p = {
  pos_fname = p.Lexing.pos_fname;
  pos_lnum  = p.Lexing.pos_lnum;
  pos_bol   = p.Lexing.pos_bol;
  pos_cnum  = p.Lexing.pos_cnum;
}

let externalize p = {
  Lexing.pos_fname = p.pos_fname;
  pos_lnum  = p.pos_lnum;
  pos_bol   = p.pos_bol;
  pos_cnum  = p.pos_cnum;
}

let with_poss p1 p2 v =
  with_pos { start_p = internalize p1; end_p = internalize p2 } v

module NameSet = Set.Make (struct
  type t = name
  let compare (Name s1) (Name s2) = String.compare s1 s2
end)

let start_of_position p = p.start_p

let end_of_position p = p.end_p

let filename_of_position p =
  p.start_p.pos_fname

let line p =
  p.pos_lnum

let column p =
  p.pos_cnum - p.pos_bol

let characters p1 p2 =
  (column p1, p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let string_of_lexing_position p =
  let c = p.pos_cnum - p.pos_bol in
  (string_of_int p.pos_lnum)^":"^(string_of_int c)

let string_of_position p =
  let filename = filename_of_position p in
  let l = line p.start_p in
  let c1, c2 = characters p.start_p p.end_p in
    if filename = "" then
      Printf.sprintf "Line %d, characters %d-%d" l c1 c2
    else
      Printf.sprintf "File \"%s\", line %d, characters %d-%d" filename l c1 c2

let compare_positions p1 p2 =
  compare p1.start_p.pos_cnum p2.start_p.pos_cnum
