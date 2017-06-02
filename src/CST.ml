(**

    A concrete syntax tree.

    This tree refers directly to the grammar production rules.

    We use the following convention to name data constructors: the
    constructor starts with the name of the non terminal and continues
    with the names of producers appearing in the right-hand-side of
    the production rule.

    We do not need types for operators and reserved word.
 *)

type complete_command =
  | CompleteCommand_CList_Separator of clist * separator
  | CompleteCommand_CList of clist

 and clist =
   (* called [list] in the grammar *)
   | CList_CList_SeparatorOp_AndOr of clist * separator_op * and_or
   | CList_AndOr of and_or

 and and_or =
   | AndOr_Pipeline of pipeline
   | AndOr_AndOr_AndIf_LineBreak_Pipeline of and_or * linebreak * pipeline
   | AndOr_AndOr_OrIf_LineBreak_Pipeline  of and_or * linebreak * pipeline

 and pipeline =
   | Pipeline_PipeSequence of pipe_sequence
   | Pipeline_Bang_PipeSequence of pipe_sequence

 and pipe_sequence =
   | PipeSequence_Command of command
   | PipeSequence_PipeSequence_Pipe_LineBreak_Command of
       pipe_sequence * linebreak * command

 and command =
   | Command_SimpleCommand of simple_command
   | Command_CompoundCommand of compound_command
   | Command_CompoundCommand_RedirectList of compound_command * redirect_list
   | Command_FunctionDefinition of function_definition

 and compound_command =
   | CompoundCommand_BraceGroup of brace_group
   | CompoundCommand_Subshell of subshell
   | CompoundCommand_ForClause of for_clause
   | CompoundCommand_CaseClause of case_clause
   | CompoundCommand_IfClause of if_clause
   | CompoundCommand_WhileClause of while_clause
   | CompoundCommand_UntilClause of until_clause

 and subshell =
   | Subshell_Lparen_CompoundList_Rparen of compound_list

 and compound_list =
   | CompoundList_Term of term
   | CompoundList_NewLineList_Term of newline_list * term
   | CompoundList_Term_Separator of term * separator
   | CompoundList_NewLineList_Term_Separator of newline_list * term * separator

 and term =
   | Term_Term_Separator_AndOr of term * separator * and_or
   | Term_AndOr of and_or

 and for_clause =
   | ForClause_For_Name_LineBreak_DoGroup of name * linebreak * do_group
   | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup of
       name * linebreak * sequential_sep * do_group
   | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup of
       name * linebreak * wordlist * sequential_sep * do_group

 and name =
   | Name of SemanticValues.name

 (* no type for [in] since it is just a reserved word. *)

 and wordlist =
   | WordList_WordList_Word of wordlist * SemanticValues.word
   | WordList_Word of SemanticValues.word

 and case_clause =
   | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac of
       SemanticValues.word * linebreak * linebreak * case_list
   | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac of
       SemanticValues.word * linebreak * linebreak * case_list_ns
   | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac of
       SemanticValues.word * linebreak * linebreak

 and case_list_ns =
   | CaseListNS_CaseList_CaseItemNS of case_list * case_item_ns
   | CaseListNS_CaseItemNS of case_item_ns

 and case_list =
   | CaseList_CaseList_CaseItem of case_list * case_item
   | CaseList_CaseItem of case_item

 and case_item_ns =
   | CaseItemNS_Pattern_Rparen_LineBreak of
       pattern * linebreak
   | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak of
       pattern * compound_list * linebreak
   | CaseItemNS_Lparen_Pattern_Rparen_LineBreak of
       pattern * linebreak
   | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak of
       pattern * compound_list * linebreak

 and case_item =
   | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak of
       pattern * linebreak * linebreak
   | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak of
       pattern * compound_list * linebreak
   | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak of
       pattern * linebreak * linebreak
   | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak of
       pattern * compound_list * linebreak

 and pattern =
   | Pattern_Word of SemanticValues.word
   | Pattern_Pattern_Pipe_Word of pattern * SemanticValues.word

 and if_clause =
   | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi of
       compound_list * compound_list * else_part
   | IfClause_If_CompoundList_Then_CompoundList_Fi of
       compound_list * compound_list

 and else_part =
   | ElsePart_Elif_CompoundList_Then_CompoundList of
       compound_list * compound_list
   | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart of
       compound_list * compound_list * else_part
   | ElsePart_Else_CompoundList of
       compound_list

 and while_clause =
   | WhileClause_While_CompoundList_DoGroup of
       compound_list * do_group

 and until_clause =
   | UntilClause_Until_CompoundList_DoGroup of
       compound_list * do_group

 and function_definition =
   | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody of
       fname * linebreak * function_body

 and function_body =
   | FunctionBody_CompoundCommand of
       compound_command
   | FunctionBody_CompoundCommand_RedirectList of
       compound_command * redirect_list

 and fname =
   | Fname_Name of SemanticValues.name

 and brace_group =
   | BraceGroup_LBrace_CompoundList_RBrace of compound_list

 and do_group =
   | DoGroup_Do_CompoundList_Done of compound_list

 and simple_command =
   | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix of
       cmd_prefix * cmd_word * cmd_suffix
   | SimpleCommand_CmdPrefix_CmdWord of
       cmd_prefix * cmd_word
   | SimpleCommand_CmdPrefix of
       cmd_prefix
   | SimpleCommand_CmdName_CmdSuffix of
       cmd_name * cmd_suffix
   | SimpleCommand_CmdName of
       cmd_name

 and cmd_name =
   | CmdName_Word of SemanticValues.word

 and cmd_word =
   | CmdWord_Word of SemanticValues.word

 and cmd_prefix =
   | CmdPrefix_IoRedirect of
       io_redirect
   | CmdPrefix_CmdPrefix_IoRedirect of
       cmd_prefix * io_redirect
   | CmdPrefix_AssignmentWord of
       SemanticValues.word SemanticValues.assignment_word
   | CmdPrefix_CmdPrefix_AssignmentWord of
       cmd_prefix * SemanticValues.word SemanticValues.assignment_word

 and cmd_suffix =
   | CmdSuffix_IoRedirect of io_redirect
   | CmdSuffix_CmdSuffix_IoRedirect of cmd_suffix * io_redirect
   | CmdSuffix_Word of SemanticValues.word
   | CmdSuffix_CmdSuffix_Word of cmd_suffix * SemanticValues.word

 and redirect_list =
   | RedirectList_IoRedirect of io_redirect
   | RedirectList_RedirectList_IoRedirect of redirect_list * io_redirect

 and io_redirect =
   | IoRedirect_IoFile of io_file
   | IoRedirect_IoNumber_IoFile of SemanticValues.io_number * io_file
   | IoRedirect_IoHere of io_here
   | IoRedirect_IoNumber_IoHere of SemanticValues.io_number * io_here

 and io_file =
   | IoFile_Less_FileName of filename
   | IoFile_LessAnd_FileName of filename
   | IoFile_Great_FileName of filename
   | IoFile_GreatAnd_FileName of filename
   | IoFile_DGreat_FileName of filename
   | IoFile_LessGreat_FileName of filename
   | IoFile_Clobber_FileName of filename

 and filename =
   | Filename_Word of SemanticValues.word

 and io_here =
   | IoHere_DLess_HereEnd of here_end
   | IoHere_DLessDash_HereEnd of here_end

 and here_end =
   | HereEnd_Word of SemanticValues.word

 and newline_list =
   | NewLineList_NewLine
   | NewLineList_NewLineList_NewLine of newline_list

 and linebreak =
   | LineBreak_NewLineList of newline_list
   | LineBreak_Empty

 and separator_op =
   | SeparatorOp_Uppersand
   | SeparatorOp_Semicolon

 and separator =
   | Separator_SeparatorOp_LineBreak of
       separator_op * linebreak
   | Separator_NewLineList of
       newline_list

 and sequential_sep =
   | SequentialSep_Semicolon_LineBreak of
       linebreak
   | SequentialSep_NewLineList of
       newline_list

[@@deriving yojson]

type complete_command_list = complete_command list  [@@deriving yojson]

let complete_command_to_json c =
  complete_command_to_yojson c

let complete_command_list_to_json cl =
  complete_command_list_to_yojson cl


class iterator = object(self)

  method on_complete_command = function
    | CompleteCommand_CList_Separator (l, s) ->
       self#on_clist l;
       self#on_separator s
    | CompleteCommand_CList l ->
       self#on_clist l

  method on_clist = function
    | CList_CList_SeparatorOp_AndOr (c, s, a) ->
       self#on_clist c;
       self#on_separator_op s;
       self#on_and_or a
    | CList_AndOr a ->
       self#on_and_or a

  method on_and_or = function
    | AndOr_Pipeline p ->
       self#on_pipeline p
    | AndOr_AndOr_AndIf_LineBreak_Pipeline (a, l, p) ->
       self#on_and_or a;
       self#on_linebreak l;
       self#on_pipeline p
    | AndOr_AndOr_OrIf_LineBreak_Pipeline (a, l, p) ->
       self#on_and_or a;
       self#on_linebreak l;
       self#on_pipeline p

  method on_pipeline = function
    | Pipeline_PipeSequence p ->
       self#on_pipe_sequence p
    | Pipeline_Bang_PipeSequence p ->
       self#on_pipe_sequence p

  method on_pipe_sequence = function
    | PipeSequence_Command c ->
       self#on_command c
    | PipeSequence_PipeSequence_Pipe_LineBreak_Command (p, l, c) ->
       self#on_pipe_sequence p;
       self#on_linebreak l;
       self#on_command c

  method on_command = function
    | Command_SimpleCommand s ->
       self#on_simple_command s
    | Command_CompoundCommand c ->
       self#on_compound_command c
    | Command_CompoundCommand_RedirectList (c, r) ->
       self#on_compound_command c;
       self#on_redirect_list r
    | Command_FunctionDefinition f ->
       self#on_function_definition f

  method on_compound_command = function
    | CompoundCommand_BraceGroup b ->
       self#on_brace_group b
    | CompoundCommand_Subshell s ->
       self#on_subshell s
    | CompoundCommand_ForClause f ->
       self#on_for_clause f
    | CompoundCommand_CaseClause c ->
       self#on_case_clause c
    | CompoundCommand_IfClause i ->
       self#on_if_clause i
    | CompoundCommand_WhileClause w ->
       self#on_while_clause w
    | CompoundCommand_UntilClause u ->
       self#on_until_clause u

  method on_subshell = function
    | Subshell_Lparen_CompoundList_Rparen c ->
       self#on_compound_list c

  method on_compound_list = function
    | CompoundList_Term t ->
       self#on_term t
    | CompoundList_NewLineList_Term (n, t) ->
       self#on_newline_list n;
       self#on_term t
    | CompoundList_Term_Separator (t, s) ->
       self#on_term t;
       self#on_separator s
    | CompoundList_NewLineList_Term_Separator (n, t, s) ->
       self#on_newline_list n;
       self#on_term t;
       self#on_separator s

  method on_term = function
    | Term_Term_Separator_AndOr (t, s, a) ->
       self#on_term t;
       self#on_separator s;
       self#on_and_or a
    | Term_AndOr a ->
       self#on_and_or a

  method on_for_clause = function
    | ForClause_For_Name_LineBreak_DoGroup (n, l, d) ->
       self#on_name n;
       self#on_linebreak l;
       self#on_do_group d
    | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (n, l, s, d) ->
       self#on_name n;
       self#on_linebreak l;
       self#on_sequential_sep s;
       self#on_do_group d
    | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (n, l, w, s, d) ->
       self#on_name n;
       self#on_linebreak l;
       self#on_wordlist w;
       self#on_sequential_sep s;
       self#on_do_group d

  method on_name = function
    | Name n ->
       ()

  method on_wordlist = function
    | WordList_WordList_Word (w, _) ->
       self#on_wordlist w
    | WordList_Word _ ->
       ()

  method on_case_clause = function
    | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (_, l1, l2, c) ->
       self#on_linebreak l1;
       self#on_linebreak l2;
       self#on_case_list c
    | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (_,l1, l2, c) ->
       self#on_linebreak l1;
       self#on_linebreak l2;
       self#on_case_list_ns c
    | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (_, l1, l2) ->
       self#on_linebreak l1;
       self#on_linebreak l2

  method on_case_list_ns = function
    | CaseListNS_CaseList_CaseItemNS (c, ci) ->
       self#on_case_list c;
       self#on_case_item_ns ci
    | CaseListNS_CaseItemNS ci ->
       self#on_case_item_ns ci

  method on_case_list = function
    | CaseList_CaseList_CaseItem (c, ci) ->
       self#on_case_list c;
       self#on_case_item ci
    | CaseList_CaseItem ci ->
       self#on_case_item ci

  method on_case_item_ns = function
    | CaseItemNS_Pattern_Rparen_LineBreak (p, l) ->
       self#on_pattern p;
       self#on_linebreak l
    | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (p, c, l) ->
       self#on_pattern p;
       self#on_compound_list c;
       self#on_linebreak l
    | CaseItemNS_Lparen_Pattern_Rparen_LineBreak (p, l) ->
       self#on_pattern p;
       self#on_linebreak l
    | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (p, c, l) ->
       self#on_pattern p;
       self#on_compound_list c;
       self#on_linebreak l

  method on_case_item = function
    | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2) ->
       self#on_pattern p;
       self#on_linebreak l1;
       self#on_linebreak l2
    | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l) ->
       self#on_pattern p;
       self#on_compound_list c;
       self#on_linebreak l
    | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2) ->
       self#on_pattern p;
       self#on_linebreak l1;
       self#on_linebreak l2
    | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l) ->
       self#on_pattern p;
       self#on_compound_list c;
       self#on_linebreak l

  method on_pattern = function
    | Pattern_Word _ ->
       ()
    | Pattern_Pattern_Pipe_Word (p, _) ->
       self#on_pattern p

  method on_if_clause = function
    | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (c1, c2, e) ->
       self#on_compound_list c1;
       self#on_compound_list c2;
       self#on_else_part e
    | IfClause_If_CompoundList_Then_CompoundList_Fi (c1, c2) ->
       self#on_compound_list c1;
       self#on_compound_list c2

  method on_else_part = function
    | ElsePart_Elif_CompoundList_Then_CompoundList (c1, c2) ->
       self#on_compound_list c1;
       self#on_compound_list c2
    | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (c1, c2, e) ->
       self#on_compound_list c1;
       self#on_compound_list c2;
       self#on_else_part e
    | ElsePart_Else_CompoundList c ->
       self#on_compound_list c

  method on_while_clause = function
    | WhileClause_While_CompoundList_DoGroup (c, d) ->
       self#on_compound_list c;
       self#on_do_group d

  method on_until_clause = function
    | UntilClause_Until_CompoundList_DoGroup (c, d) ->
       self#on_compound_list c;
       self#on_do_group d

  method on_function_definition = function
    | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (f, l, b) ->
       self#on_fname f;
       self#on_linebreak l;
       self#on_function_body b

  method on_function_body = function
    | FunctionBody_CompoundCommand c ->
       self#on_compound_command c
    | FunctionBody_CompoundCommand_RedirectList (c, r) ->
       self#on_compound_command c;
       self#on_redirect_list r

  method on_fname = function
    | Fname_Name _ ->
       ()

  method on_brace_group = function
    | BraceGroup_LBrace_CompoundList_RBrace c ->
       self#on_compound_list c

  method on_do_group = function
    | DoGroup_Do_CompoundList_Done c ->
       self#on_compound_list c

  method on_simple_command = function
    | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, cs) ->
       self#on_cmd_prefix cp;
       self#on_cmd_word cw;
       self#on_cmd_suffix cs
    | SimpleCommand_CmdPrefix_CmdWord (cp, cw) ->
       self#on_cmd_prefix cp;
       self#on_cmd_word cw
    | SimpleCommand_CmdPrefix cp ->
       self#on_cmd_prefix cp
    | SimpleCommand_CmdName_CmdSuffix (cn, cs) ->
       self#on_cmd_name cn;
       self#on_cmd_suffix cs
    | SimpleCommand_CmdName cn ->
       self#on_cmd_name cn

  method on_cmd_name = function
    | CmdName_Word _ ->
       ()

  method on_cmd_word = function
    | CmdWord_Word _ ->
       ()

  method on_cmd_prefix = function
    | CmdPrefix_IoRedirect i ->
       self#on_io_redirect i
    | CmdPrefix_CmdPrefix_IoRedirect (cp, i) ->
       self#on_cmd_prefix cp;
       self#on_io_redirect i
    | CmdPrefix_AssignmentWord _ ->
       ()
    | CmdPrefix_CmdPrefix_AssignmentWord (cp, _) ->
       self#on_cmd_prefix cp

  method on_cmd_suffix = function
    | CmdSuffix_IoRedirect i ->
       self#on_io_redirect i
    | CmdSuffix_CmdSuffix_IoRedirect (cs, i) ->
       self#on_cmd_suffix cs;
       self#on_io_redirect i
    | CmdSuffix_Word _ ->
       ()
    | CmdSuffix_CmdSuffix_Word (cs, _) ->
       self#on_cmd_suffix cs

  method on_redirect_list = function
    | RedirectList_IoRedirect i ->
       self#on_io_redirect i
    | RedirectList_RedirectList_IoRedirect (r, i) ->
       self#on_redirect_list r;
       self#on_io_redirect i

  method on_io_redirect = function
    | IoRedirect_IoFile i ->
       self#on_io_file i
    | IoRedirect_IoNumber_IoFile (_, i) ->
       self#on_io_file i
    | IoRedirect_IoHere i ->
       self#on_io_here i
    | IoRedirect_IoNumber_IoHere (_, i) ->
       self#on_io_here i

  method on_io_file = function
    | IoFile_Less_FileName f ->
       self#on_filename f
    | IoFile_LessAnd_FileName f ->
       self#on_filename f
    | IoFile_Great_FileName f ->
       self#on_filename f
    | IoFile_GreatAnd_FileName f ->
       self#on_filename f
    | IoFile_DGreat_FileName f ->
       self#on_filename f
    | IoFile_LessGreat_FileName f ->
       self#on_filename f
    | IoFile_Clobber_FileName f ->
       self#on_filename f

  method on_filename = function
    | Filename_Word _ ->
       ()

  method on_io_here = function
    | IoHere_DLess_HereEnd h ->
       self#on_here_end h
    | IoHere_DLessDash_HereEnd h ->
       self#on_here_end h

  method on_here_end = function
    | HereEnd_Word _ ->
       ()

  method on_newline_list = function
    | NewLineList_NewLine ->
       ()
    | NewLineList_NewLineList_NewLine n ->
       self#on_newline_list n

  method on_linebreak = function
    | LineBreak_NewLineList n ->
       self#on_newline_list n
    | LineBreak_Empty ->
       ()

  method on_separator_op = function
    | SeparatorOp_Uppersand ->
       ()
    | SeparatorOp_Semicolon ->
       ()

  method on_separator = function
    | Separator_SeparatorOp_LineBreak (s, l) ->
       self#on_separator_op s;
       self#on_linebreak l
    | Separator_NewLineList n ->
       self#on_newline_list n

  method on_sequential_sep = function
    | SequentialSep_Semicolon_LineBreak n ->
       self#on_linebreak n
    | SequentialSep_NewLineList n ->
       self#on_newline_list n
end

(* Printer *)

open Format

let pp_constructor0 ppf s =
  fprintf ppf "%s" s

let pp_constructor1 ppf s pp1 e1 : unit =
  fprintf ppf "%s (@\n@[<h 2>  %a@])" s pp1 e1

let pp_constructor2 ppf s pp1 e1 pp2 e2 =
  fprintf ppf "%s (@\n@[<h 2>  %a@],@\n@[<h 2>  %a@])" s pp1 e1 pp2 e2

let pp_constructor3 ppf s pp1 e1 pp2 e2 pp3 e3 =
  fprintf ppf "%s (@\n@[<h 2>  %a@],@\n@[<h 2>  %a@],@\n@[<h 2>  %a@])" s pp1 e1 pp2 e2 pp3 e3

let pp_constructor4 ppf s pp1 e1 pp2 e2 pp3 e3 pp4 e4 =
  fprintf ppf "%s (@\n@[<h 2>  %a@],@\n@[<h 2>  %a@],@\n@[<h 2>  %a@],@\n@[<h 2>  %a@])" s pp1 e1 pp2 e2 pp3 e3 pp4 e4

let pp_constructor5 ppf s pp1 e1 pp2 e2 pp3 e3 pp4 e4 pp5 e5 =
  fprintf ppf "%s (@\n@[<h 2>  %a@],@\n@[<h 2>  %a@],@\n@[<h 2>  %a@],@\n@[<h 2>  %a@],@\n@[<h 2>  %a@])" s pp1 e1 pp2 e2 pp3 e3 pp4 e4 pp5 e5

let rec pp_complete_command ppf = function
  | CompleteCommand_CList_Separator (l, s) ->
     pp_constructor2 ppf "CompleteCommand_CList_Separator" pp_clist l pp_separator s
  | CompleteCommand_CList l ->
     pp_constructor1 ppf "CompleteCommand_Clist" pp_clist l

and pp_clist ppf = function
  | CList_CList_SeparatorOp_AndOr (c, s, a) ->
     pp_constructor3 ppf "CList_CList_SeparatorOp_AndOr" pp_clist c pp_separator_op s pp_and_or a
  | CList_AndOr a ->
     pp_constructor1 ppf "CList_AndOr" pp_and_or a

and pp_and_or ppf = function
  | AndOr_Pipeline p ->
     pp_constructor1 ppf "AndOr_Pipeline" pp_pipeline p
  | AndOr_AndOr_AndIf_LineBreak_Pipeline (a, l, p) ->
     pp_constructor3 ppf "AndOr_AndOr_AndIf_LineBreak_Pipeline" pp_and_or a pp_linebreak l pp_pipeline p
  | AndOr_AndOr_OrIf_LineBreak_Pipeline (a, l, p) ->
     pp_constructor3 ppf "AndOr_AndOr_OrIf_LineBreak_Pipeline" pp_and_or a pp_linebreak l pp_pipeline p

and pp_pipeline ppf = function
  | Pipeline_PipeSequence p ->
     pp_constructor1 ppf "Pipeline_PipeSequence" pp_pipe_sequence p
  | Pipeline_Bang_PipeSequence p ->
     pp_constructor1 ppf "Pipeline_Bang_PipeSequence" pp_pipe_sequence p

and pp_pipe_sequence ppf = function
  | PipeSequence_Command c ->
     pp_constructor1 ppf "PipeSequence_Command" pp_command c
  | PipeSequence_PipeSequence_Pipe_LineBreak_Command (p, l, c) ->
     pp_constructor3 ppf "PipeSequence_PipeSequence_Pipe_LineBreak_Command" pp_pipe_sequence p pp_linebreak l pp_command c

and pp_command ppf = function
  | Command_SimpleCommand s ->
     pp_constructor1 ppf "Command_SimpleCommand" pp_simple_command s
  | Command_CompoundCommand c ->
     pp_constructor1 ppf "Command_CompoundCommand" pp_compound_command c
  | Command_CompoundCommand_RedirectList (c, r) ->
     pp_constructor2 ppf "Command_CompoundCommand_RedirectList" pp_compound_command c pp_redirect_list r
  | Command_FunctionDefinition f ->
     pp_constructor1 ppf "Command_FunctionDefinition" pp_function_definition f

and pp_compound_command ppf = function
  | CompoundCommand_BraceGroup b ->
     pp_constructor1 ppf "CompoundCommand_BraceGroup" pp_brace_group b
  | CompoundCommand_Subshell s ->
     pp_constructor1 ppf "CompoundCommand_Subshell" pp_subshell s
  | CompoundCommand_ForClause f ->
     pp_constructor1 ppf "CompoundCommand_ForClause" pp_for_clause f
  | CompoundCommand_CaseClause c ->
     pp_constructor1 ppf "CompoundCommand_ClaseClause" pp_case_clause c
  | CompoundCommand_IfClause i ->
     pp_constructor1 ppf "CompoundCommand_IfClause" pp_if_clause i
  | CompoundCommand_WhileClause w ->
     pp_constructor1 ppf "CompoundCommand_WhileClause" pp_while_clause w
  | CompoundCommand_UntilClause u ->
     pp_constructor1 ppf "CompoundCommand_UntilClause" pp_until_clause u

and pp_subshell ppf = function
  | Subshell_Lparen_CompoundList_Rparen c ->
     pp_constructor1 ppf "Subshell_Lparen_CompoundList_Rparen" pp_compound_list c

and pp_compound_list ppf = function
  | CompoundList_Term t ->
     pp_constructor1 ppf "CompoundList_Term" pp_term t
  | CompoundList_NewLineList_Term (n, t) ->
     pp_constructor2 ppf "CompoundList_NewLineList_Term" pp_newline_list n pp_term t
  | CompoundList_Term_Separator (t, s) ->
     pp_constructor2 ppf "CompoundList_Term_Separator" pp_term t pp_separator s
  | CompoundList_NewLineList_Term_Separator (n, t, s) ->
     pp_constructor3 ppf "CompoundList_NewLineList_Term_Separator" pp_newline_list n pp_term t pp_separator s

and pp_term ppf = function
  | Term_Term_Separator_AndOr (t, s, a) ->
     pp_constructor3 ppf "Term_Term_Separator_AndOr" pp_term t pp_separator s pp_and_or a
  | Term_AndOr a ->
     pp_constructor1 ppf "Term_AndOr" pp_and_or a

and pp_for_clause ppf = function
  | ForClause_For_Name_LineBreak_DoGroup (n, l, d) ->
     pp_constructor3 ppf "ForClause_For_Name_LineBreak_DoGroup" pp_name n pp_linebreak l pp_do_group d
  | ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (n, l, s, d) ->
     pp_constructor4 ppf "ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup" pp_name n pp_linebreak l pp_sequential_sep s pp_do_group d
  | ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (n, l, w, s, d) ->
     pp_constructor5 ppf "ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup" pp_name n pp_linebreak l pp_wordlist w pp_sequential_sep s pp_do_group d

and pp_name ppf = function
  | Name n ->
     pp_constructor1 ppf "Name" SemanticValues.pp_name n

and pp_wordlist ppf = function
  | WordList_WordList_Word (wl, w) ->
     pp_constructor2 ppf "WordList_WordList_Word" pp_wordlist wl SemanticValues.pp_word w
  | WordList_Word w ->
     pp_constructor1 ppf "WordList_Word" SemanticValues.pp_word w

and pp_case_clause ppf = function
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (w, l1, l2, c) ->
     pp_constructor4 ppf "CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac" SemanticValues.pp_word w pp_linebreak l1 pp_linebreak l2 pp_case_list c
  | CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (w, l1, l2, c) ->
     pp_constructor4 ppf "CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac" SemanticValues.pp_word w pp_linebreak l1 pp_linebreak l2 pp_case_list_ns c
  | CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (w, l1, l2) ->
     pp_constructor3 ppf "CaseClause_Case_Word_LineBreak_In_LineBreak_Esac" SemanticValues.pp_word w pp_linebreak l1 pp_linebreak l2

and pp_case_list_ns ppf = function
  | CaseListNS_CaseList_CaseItemNS (c, ci) ->
     pp_constructor2 ppf "CaseListNS_CaseList_CaseItemNS" pp_case_list c pp_case_item_ns ci
  | CaseListNS_CaseItemNS ci ->
     pp_constructor1 ppf "CaseListNS_CaseItemNS" pp_case_item_ns ci

and pp_case_list ppf = function
  | CaseList_CaseList_CaseItem (c, ci) ->
     pp_constructor2 ppf "CaseList_CaseList_CaseItem" pp_case_list c pp_case_item ci
  | CaseList_CaseItem ci ->
     pp_constructor1 ppf "CaseList_CaseItem" pp_case_item ci

and pp_case_item_ns ppf = function
  | CaseItemNS_Pattern_Rparen_LineBreak (p, l) ->
     pp_constructor2 ppf "CaseItemNS_Pattern_Rparen_LineBreak" pp_pattern p pp_linebreak l
  | CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (p, c, l) ->
     pp_constructor3 ppf "CaseItemNS_Pattern_Rparen_CompoundList_LineBreak" pp_pattern p pp_compound_list c pp_linebreak l
  | CaseItemNS_Lparen_Pattern_Rparen_LineBreak (p, l) ->
     pp_constructor2 ppf "CaseItemNS_Lparen_Pattern_Rparen_LineBreak" pp_pattern p pp_linebreak l
  | CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (p, c, l) ->
     pp_constructor3 ppf "CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak" pp_pattern p pp_compound_list c pp_linebreak l

and pp_case_item ppf = function
   | CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2) ->
      pp_constructor3 ppf "CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak" pp_pattern p pp_linebreak l1 pp_linebreak l2
   | CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l) ->
      pp_constructor3 ppf "CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak" pp_pattern p pp_compound_list c pp_linebreak l
   | CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2) ->
      pp_constructor3 ppf "CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak" pp_pattern p pp_linebreak l1 pp_linebreak l2
   | CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l) ->
      pp_constructor3 ppf "CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak" pp_pattern p pp_compound_list c pp_linebreak l

and pp_pattern ppf = function
  | Pattern_Word w ->
     pp_constructor1 ppf "Pattern_Word" SemanticValues.pp_word w
  | Pattern_Pattern_Pipe_Word (p, w) ->
     pp_constructor2 ppf "Pattern_Pattern_Pipe_Word" pp_pattern p SemanticValues.pp_word w

and pp_if_clause ppf = function
  | IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (c1, c2, e) ->
     pp_constructor3 ppf "IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi" pp_compound_list c1 pp_compound_list c2 pp_else_part e
  | IfClause_If_CompoundList_Then_CompoundList_Fi (c1, c2) ->
     pp_constructor2 ppf "IfClause_If_CompoundList_Then_CompoundList_Fi" pp_compound_list c1 pp_compound_list c2

and pp_else_part ppf = function
  | ElsePart_Elif_CompoundList_Then_CompoundList (c1, c2) ->
     pp_constructor2 ppf "ElsePart_Elif_CompoundList_Then_CompoundList" pp_compound_list c1 pp_compound_list c2
  | ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (c1, c2, e) ->
     pp_constructor3 ppf "ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart" pp_compound_list c1 pp_compound_list c2 pp_else_part e
  | ElsePart_Else_CompoundList c ->
     pp_constructor1 ppf "ElsePart_Else_CompoundList" pp_compound_list c

and pp_while_clause ppf x = ()
(* and pp_while_clause ppf = function *)
(*   | WhileClause_While_CompoundList_DoGroup (c, d) -> *)
(*      pp_compound_list c; *)
(*      pp_do_group d *)

and pp_until_clause ppf x = ()
(* and pp_until_clause ppf = function *)
(*   | UntilClause_Until_CompoundList_DoGroup (c, d) -> *)
(*      pp_compound_list c; *)
(*      pp_do_group d *)

and pp_function_definition ppf x = ()
(* and pp_function_definition ppf = function *)
(*   | FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (f, l, b) -> *)
(*      pp_fname f; *)
(*      pp_linebreak l; *)
(*      pp_function_body b *)

(* and pp_function_body ppf = function *)
(*   | FunctionBody_CompoundCommand c -> *)
(*      pp_compound_command c *)
(*   | FunctionBody_CompoundCommand_RedirectList (c, r) -> *)
(*      pp_compound_command c; *)
(*      pp_redirect_list r *)

(* and pp_fname ppf = function *)
(*   | Fname_Name _ -> *)
(*      () *)

and pp_brace_group ppf x = ()
(* and pp_brace_group ppf = function *)
(*   | BraceGroup_LBrace_CompoundList_RBrace c -> *)
(*      pp_compound_list c *)

and pp_do_group ppf = function
  | DoGroup_Do_CompoundList_Done c ->
     pp_constructor1 ppf "DoGroup_Do_CompoundList_Done" pp_compound_list c

and pp_simple_command ppf = function
  | SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, cs) ->
     pp_constructor3 ppf "SimpleCommand_CmdPrefix_CmdWord_CmdSuffix" pp_cmd_prefix cp pp_cmd_word cw pp_cmd_suffix cs
  | SimpleCommand_CmdPrefix_CmdWord (cp, cw) ->
     pp_constructor2 ppf "SimpleCommand_CmdPrefix_CmdWord" pp_cmd_prefix cp pp_cmd_word cw
  | SimpleCommand_CmdPrefix cp ->
     pp_constructor1 ppf "SimpleCommand_CmdPrefix" pp_cmd_prefix cp
  | SimpleCommand_CmdName_CmdSuffix (cn, cs) ->
     pp_constructor2 ppf "SimpleCommand_CmdName_CmdSuffix" pp_cmd_name cn pp_cmd_suffix cs
  | SimpleCommand_CmdName cn ->
     pp_constructor1 ppf "SimpleCommand_CmdName" pp_cmd_name cn

and pp_cmd_name ppf = function
  | CmdName_Word w ->
     pp_constructor1 ppf "CmdName_Word" SemanticValues.pp_word w

and pp_cmd_word ppf = function
  | CmdWord_Word w ->
     pp_constructor1 ppf "CmdWord_Word" SemanticValues.pp_word w

and pp_cmd_prefix ppf = function
  | CmdPrefix_IoRedirect i ->
     pp_constructor1 ppf "CmdPrefix_IoRedirect" pp_io_redirect i
  | CmdPrefix_CmdPrefix_IoRedirect (cp, i) ->
     pp_constructor2 ppf "CmdPrefix_CmdPrefix_IoRedirect" pp_cmd_prefix cp pp_io_redirect i
  | CmdPrefix_AssignmentWord aw ->
     pp_constructor1 ppf "CmdPrefix_AssignmentWord" SemanticValues.pp_assignment_word aw
  | CmdPrefix_CmdPrefix_AssignmentWord (cp, aw) ->
     pp_constructor2 ppf "CmdPrefix_CmdPrefix_AssignmentWord" pp_cmd_prefix cp SemanticValues.pp_assignment_word aw

and pp_cmd_suffix ppf = function
  | CmdSuffix_IoRedirect i ->
     pp_constructor1 ppf "CmdSuffix_IoRedirect" pp_io_redirect i
  | CmdSuffix_CmdSuffix_IoRedirect (cs, i) ->
     pp_constructor2 ppf "CmdSuffix_CmdSuffix_IoRedirect" pp_cmd_suffix cs pp_io_redirect i
  | CmdSuffix_Word w ->
     pp_constructor1 ppf "CmdSuffix_Word" SemanticValues.pp_word w
  | CmdSuffix_CmdSuffix_Word (cs, w) ->
     pp_constructor2 ppf "CmdSuffix_CmdSuffix_Word" pp_cmd_suffix cs SemanticValues.pp_word w

and pp_redirect_list ppf x = ()
(* and pp_redirect_list ppf = function *)
(*   | RedirectList_IoRedirect i -> *)
(*      pp_io_redirect i *)
(*   | RedirectList_RedirectList_IoRedirect (r, i) -> *)
(*      pp_redirect_list r; *)
(*      pp_io_redirect i *)

and pp_io_redirect ppf = function
  | IoRedirect_IoFile i ->
     pp_constructor1 ppf "IoRedirect_IoFile" pp_io_file i
  | IoRedirect_IoNumber_IoFile (n, i) ->
     pp_constructor2 ppf "IoRedirect_IoNumber_IoFile" SemanticValues.pp_io_number n pp_io_file i
  | IoRedirect_IoHere i ->
     pp_constructor1 ppf "IoRedirect_IoHere" pp_io_here i
  | IoRedirect_IoNumber_IoHere (n, i) ->
     pp_constructor2 ppf "IoRedirect_IoNumber_IoHere" SemanticValues.pp_io_number n pp_io_here i

and pp_io_file ppf = function
  | IoFile_Less_FileName f ->
     pp_constructor1 ppf "IoFile_Less_FileName" pp_filename f
  | IoFile_LessAnd_FileName f ->
     pp_constructor1 ppf "IoFile_LessAnd_FileName" pp_filename f
  | IoFile_Great_FileName f ->
     pp_constructor1 ppf "IoFile_Great_FileName" pp_filename f
  | IoFile_GreatAnd_FileName f ->
     pp_constructor1 ppf "IoFile_GreatAnd_FileName" pp_filename f
  | IoFile_DGreat_FileName f ->
     pp_constructor1 ppf "IoFile_DGreat_FileName" pp_filename f
  | IoFile_LessGreat_FileName f ->
     pp_constructor1 ppf "IoFile_LessGreat_FileName" pp_filename f
  | IoFile_Clobber_FileName f ->
     pp_constructor1 ppf "IoFile_Clobber_FileName" pp_filename f

and pp_filename ppf = function
  | Filename_Word w ->
     pp_constructor1 ppf "Filename_Word" SemanticValues.pp_word w

and pp_io_here ppf = function
  | IoHere_DLess_HereEnd h ->
     pp_constructor1 ppf "IoHere_DLess_HereEnd" pp_here_end h
  | IoHere_DLessDash_HereEnd h ->
     pp_constructor1 ppf "IoHere_DLessDash_HereEnd" pp_here_end h

and pp_here_end ppf = function
  | HereEnd_Word w ->
     pp_constructor1 ppf "HereEnd_Word" SemanticValues.pp_word w

and pp_newline_list ppf = function
  | NewLineList_NewLine ->
     pp_constructor0 ppf "NewLineList_NewLine"
  | NewLineList_NewLineList_NewLine n ->
     pp_constructor1 ppf "NewLineList_NewLineList_NewLine" pp_newline_list n

and pp_linebreak ppf = function
  | LineBreak_NewLineList n ->
     pp_constructor1 ppf "LineBreak_NewLineList" pp_newline_list n
  | LineBreak_Empty ->
     pp_constructor0 ppf "LineBreak_Empty"

and pp_separator_op ppf = function
  | SeparatorOp_Uppersand ->
     pp_constructor0 ppf "SeparatorOp_Uppersand"
  | SeparatorOp_Semicolon ->
     pp_constructor0 ppf "SeparatorOp_Semicolon"

and pp_separator ppf = function
  | Separator_SeparatorOp_LineBreak (s, l) ->
     pp_constructor2 ppf "Separator_SeparatorOp_LineBreak" pp_separator_op s pp_linebreak l
  | Separator_NewLineList n ->
     pp_constructor1 ppf "Separator_NewLineList" pp_newline_list n

and pp_sequential_sep ppf = function
  | SequentialSep_Semicolon_LineBreak n ->
     pp_constructor1 ppf "SequentialSep_Semicolon_LineBreak" pp_linebreak n
  | SequentialSep_NewLineList n ->
     pp_constructor1 ppf "SequentialSep_NewLineList" pp_newline_list n


(* Sugar *)

let print_complete_command =
  ExtPervasives.pp_to_print pp_complete_command

let complete_command_to_string =
  ExtPervasives.pp_to_string pp_complete_command
