(**

    A concrete syntax tree.

    This tree refers directly to the grammar production rules.

    We use the following convention to name data constructors: the
    constructor starts with the name of the non terminal and continues
    with the names of producers appearing in the right-hand-side of
    the production rule.

    We do not need types for operators and reserved words.
 *)
type complete_command =
  | CompleteCommand_CList_Separator of clist' * separator'
  | CompleteCommand_CList of clist'

and complete_command_list = complete_command list

and clist =
  (* called [list] in the grammar *)
  | CList_CList_SeparatorOp_AndOr of clist' * separator_op' * and_or'
  | CList_AndOr of and_or'

and and_or =
  | AndOr_Pipeline of pipeline'
  | AndOr_AndOr_AndIf_LineBreak_Pipeline of and_or' * linebreak' * pipeline'
  | AndOr_AndOr_OrIf_LineBreak_Pipeline  of and_or' * linebreak' * pipeline'

and pipeline =
  | Pipeline_PipeSequence of pipe_sequence'
  | Pipeline_Bang_PipeSequence of pipe_sequence'

and pipe_sequence =
  | PipeSequence_Command of command'
  | PipeSequence_PipeSequence_Pipe_LineBreak_Command of
      pipe_sequence' * linebreak' * command'

and command =
  | Command_SimpleCommand of simple_command'
  | Command_CompoundCommand of compound_command'
  | Command_CompoundCommand_RedirectList of compound_command' * redirect_list'
  | Command_FunctionDefinition of function_definition'

and compound_command =
  | CompoundCommand_BraceGroup of brace_group'
  | CompoundCommand_Subshell of subshell'
  | CompoundCommand_ForClause of for_clause'
  | CompoundCommand_CaseClause of case_clause'
  | CompoundCommand_IfClause of if_clause'
  | CompoundCommand_WhileClause of while_clause'
  | CompoundCommand_UntilClause of until_clause'

and subshell =
  | Subshell_Lparen_CompoundList_Rparen of compound_list'

and compound_list =
  | CompoundList_Term of term'
  | CompoundList_NewLineList_Term of newline_list' * term'
  | CompoundList_Term_Separator of term' * separator'
  | CompoundList_NewLineList_Term_Separator of
      newline_list' * term' * separator'

and term =
  | Term_Term_Separator_AndOr of term' * separator' * and_or'
  | Term_AndOr of and_or'

and for_clause =
  | ForClause_For_Name_LineBreak_DoGroup of name' * linebreak' * do_group'
  | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup of
      name' * linebreak' * sequential_sep' * do_group'
  | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup of
      name' * linebreak' * wordlist' * sequential_sep' * do_group'

and wordlist =
  | WordList_WordList_Word of wordlist' * word'
  | WordList_Word of word'

and case_clause =
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac of
      word' * linebreak' * linebreak' * case_list'
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac of
      word' * linebreak' * linebreak' * case_list_ns'
  | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac of
      word' * linebreak' * linebreak'

and case_list_ns =
  | CaseListNS_CaseList_CaseItemNS of case_list' * case_item_ns'
  | CaseListNS_CaseItemNS of case_item_ns'

and case_list =
  | CaseList_CaseList_CaseItem of case_list' * case_item'
  | CaseList_CaseItem of case_item'

and case_item_ns =
  | CaseItemNS_Pattern_Rparen_LineBreak of
      pattern' * linebreak'
  | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak of
      pattern' * compound_list' * linebreak'
  | CaseItemNS_Lparen_Pattern_Rparen_LineBreak of
      pattern' * linebreak'
  | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak of
      pattern' * compound_list' * linebreak'

and case_item =
  | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak of
      pattern' * linebreak' * linebreak'
  | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak of
      pattern' * compound_list' * linebreak'
  | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak of
      pattern' * linebreak' * linebreak'
  | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak of
      pattern' * compound_list' * linebreak'

and pattern =
  | Pattern_Word of word'
  | Pattern_Pattern_Pipe_Word of pattern' * word'

and if_clause =
  | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi of
      compound_list' * compound_list' * else_part'
  | IfClause_If_CompoundList_Then_CompoundList_Fi of
      compound_list' * compound_list'

and else_part =
  | ElsePart_Elif_CompoundList_Then_CompoundList of
      compound_list' * compound_list'
  | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart of
      compound_list' * compound_list' * else_part'
  | ElsePart_Else_CompoundList of
      compound_list'

and while_clause =
  | WhileClause_While_CompoundList_DoGroup of
      compound_list' * do_group'

and until_clause =
  | UntilClause_Until_CompoundList_DoGroup of
      compound_list' * do_group'

and function_definition =
  | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody of
      fname' * linebreak' * function_body'

and function_body =
  | FunctionBody_CompoundCommand of
      compound_command'
  | FunctionBody_CompoundCommand_RedirectList of
      compound_command' * redirect_list'

and fname =
  | Fname_Name of name

and brace_group =
  | BraceGroup_LBrace_CompoundList_RBrace of compound_list'

and do_group =
  | DoGroup_Do_CompoundList_Done of compound_list'

and simple_command =
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix of
      cmd_prefix' * cmd_word' * cmd_suffix'
  | SimpleCommand_CmdPrefix_CmdWord of
      cmd_prefix' * cmd_word'
  | SimpleCommand_CmdPrefix of
      cmd_prefix'
  | SimpleCommand_CmdName_CmdSuffix of
      cmd_name' * cmd_suffix'
  | SimpleCommand_CmdName of
      cmd_name'

and cmd_name =
  | CmdName_Word of word'

and cmd_word =
  | CmdWord_Word of word'

and cmd_prefix =
  | CmdPrefix_IoRedirect of
      io_redirect'
  | CmdPrefix_CmdPrefix_IoRedirect of
      cmd_prefix' * io_redirect'
  | CmdPrefix_AssignmentWord of
      assignment_word'
  | CmdPrefix_CmdPrefix_AssignmentWord of
      cmd_prefix' * assignment_word'

and cmd_suffix =
  | CmdSuffix_IoRedirect of io_redirect'
  | CmdSuffix_CmdSuffix_IoRedirect of cmd_suffix' * io_redirect'
  | CmdSuffix_Word of word'
  | CmdSuffix_CmdSuffix_Word of cmd_suffix' * word'

and redirect_list =
  | RedirectList_IoRedirect of io_redirect'
  | RedirectList_RedirectList_IoRedirect of redirect_list' * io_redirect'

and io_redirect =
  | IoRedirect_IoFile of io_file'
  | IoRedirect_IoNumber_IoFile of io_number * io_file'
  | IoRedirect_IoHere of io_here'
  | IoRedirect_IoNumber_IoHere of io_number * io_here'

and io_file =
  | IoFile_Less_FileName of filename'
  | IoFile_LessAnd_FileName of filename'
  | IoFile_Great_FileName of filename'
  | IoFile_GreatAnd_FileName of filename'
  | IoFile_DGreat_FileName of filename'
  | IoFile_LessGreat_FileName of filename'
  | IoFile_Clobber_FileName of filename'

and filename =
  | Filename_Word of word'

and io_here =
  | IoHere_DLess_HereEnd of here_end'
  | IoHere_DLessDash_HereEnd of here_end'

and here_end =
  | HereEnd_Word of word'

and newline_list =
  | NewLineList_NewLine
  | NewLineList_NewLineList_NewLine of newline_list'

and linebreak =
  | LineBreak_NewLineList of newline_list'
  | LineBreak_Empty

and separator_op =
  | SeparatorOp_Uppersand
  | SeparatorOp_Semicolon

and separator =
  | Separator_SeparatorOp_LineBreak of
      separator_op' * linebreak'
  | Separator_NewLineList of
      newline_list'

and sequential_sep =
  | SequentialSep_Semicolon_LineBreak of
      linebreak'
  | SequentialSep_NewLineList of
      newline_list'

and word = Word of string

and name = Name of string

and assignment_word = AssignmentWord of name * word

and assignment_word' = assignment_word located

and io_number = IONumber of string

and position = {
  start_p : lexing_position;
  end_p   : lexing_position
}

and lexing_position = {
  pos_fname : string ;
  pos_lnum : int ;
  pos_bol : int ;
  pos_cnum : int ;
}

and 'a located = {
  value    : 'a;
  position : position;
}

and clist' = clist located
and and_or' = and_or located
and pipeline' = pipeline located
and pipe_sequence' = pipe_sequence located
and command' = command located
and compound_command' = compound_command located
and subshell' = subshell located
and compound_list' = compound_list located
and term' = term located
and for_clause' = for_clause located
and wordlist' = wordlist located
and case_clause' = case_clause located
and case_list_ns' = case_list_ns located
and case_list' = case_list located
and case_item_ns' = case_item_ns located
and case_item' = case_item located
and pattern' = pattern located
and if_clause' = if_clause located
and else_part' = else_part located
and while_clause' = while_clause located
and until_clause' = until_clause located
and function_definition' = function_definition located
and function_body' = function_body located
and fname' = fname located
and brace_group' = brace_group located
and do_group' = do_group located
and simple_command' = simple_command located
and cmd_name' = cmd_name located
and cmd_word' = cmd_word located
and cmd_prefix' = cmd_prefix located
and cmd_suffix' = cmd_suffix located
and redirect_list' = redirect_list located
and io_redirect' = io_redirect located
and io_file' = io_file located
and filename' = filename located
and io_here' = io_here located
and here_end' = here_end located
and newline_list' = newline_list located
and linebreak' = linebreak located
and separator_op' = separator_op located
and separator' = separator located
and sequential_sep' = sequential_sep located
and word' = word located
and name' = name located
and complete_command_list' = complete_command_list located

[@@deriving
   yojson,
   visitors { variety = "iter";    polymorphic = true },
   visitors { variety = "map";     polymorphic = true },
   visitors { variety = "reduce";  polymorphic = true },
   visitors { variety = "iter2";   polymorphic = true },
   visitors { variety = "map2";    polymorphic = true },
   visitors { variety = "reduce2"; polymorphic = true }
]

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
