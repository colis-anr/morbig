(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Nicolas Jeannerod,    *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

(**
    The type for concrete syntax trees of POSIX shell scripts. These
    trees are produced by the Morbig parser.

    These type definitions refer directly to the grammar production
    rules of the POSIX standard. We use the following convention to
    name data constructors: given a rule [A -> P1 ... PN] of the
    grammar, the constructor for this rule starts with the name of the
    non terminal A and continues with the names of producers [Pi]
    appearing in the right-hand-side of the production rule. We do not
    need types for operators and reserved words.

    These concrete syntax trees are actually richer than the
    production trees defined by the grammar. Indeed, they also embed
    concrete syntax trees for WORDs while the grammar sees WORDs are
    mere tokens. For instance, [echo `cat bar`] is interpreted by the
    grammar as a simple command with two WORDs [echo] and [`cat bar`].
    Morbig does not stop its work here: it also parses `cat bar` and
    the resulting concrete syntax tree is attached to the WORD
    [`cat bar`]. See the type definition for [word] below for more
    details.

    The PPX syntax extension package "visitors", written by François
    Pottier, is used to macro-generate many traversal functions over
    this concrete syntax tree. Note that we expose the .ml file of
    this module because the types generated by [visitors] are too
    complex to be displayed.

*)
type position = {
  start_p : lexing_position;
  end_p   : lexing_position
}

and lexing_position = Lexing.position = {
  pos_fname : string ;
  pos_lnum  : int ;
  pos_bol   : int ;
  pos_cnum  : int ;
}

and 'a located = {
  value    : 'a;
  position : position;
}
[@@deriving
   yojson,
   visitors { variety = "iter";      name = "located_iter";      polymorphic = true },
   visitors { variety = "map";       name = "located_map";       polymorphic = true },
   visitors { variety = "reduce";    name = "located_reduce";    polymorphic = true },
   visitors { variety = "mapreduce"; name = "located_mapreduce"; polymorphic = true },
   visitors { variety = "iter2";     name = "located_iter2";     polymorphic = true },
   visitors { variety = "map2";      name = "located_map2";      polymorphic = true },
   visitors { variety = "reduce2";   name = "located_reduce2";   polymorphic = true }
]

type program =
  | Program_LineBreak_CompleteCommands_LineBreak of
      linebreak' * complete_commands' * linebreak'
  | Program_LineBreak of linebreak'

and complete_commands =
  | CompleteCommands_CompleteCommands_NewlineList_CompleteCommand of
      complete_commands' * newline_list' * complete_command'
  | CompleteCommands_CompleteCommand of complete_command'

and complete_command =
  | CompleteCommand_CList_SeparatorOp of clist' * separator_op'
  | CompleteCommand_CList of clist'

and clist =
  (** This non-terminal is called [list] in the grammar but we cannot
      use this type constructor identifier because it is already
      widely used in OCaml for the standard lists. For this reason,
      we rename it into [clist]. *)
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
  | CompoundList_LineBreak_Term of linebreak' * term'
  | CompoundList_LineBreak_Term_Separator of
      linebreak' * term' * separator'

and term =
  | Term_Term_Separator_AndOr of term' * separator' * and_or'
  | Term_AndOr of and_or'

and for_clause =
  | ForClause_For_Name_DoGroup of name' * do_group'
  | ForClause_For_Name_SequentialSep_DoGroup of
      name' * sequential_sep' * do_group'
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
  | CaseItemNS_Pattern_Rparen_CompoundList of
      pattern' * compound_list'
  | CaseItemNS_Lparen_Pattern_Rparen_LineBreak of
      pattern' * linebreak'
  | CaseItemNS_Lparen_Pattern_Rparen_CompoundList of
      pattern' * compound_list'

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

(** The two IoHere constructors have two arguments. The second argument is
    the word holding the contents of the here document, which does not figure
    in the grammar.
 *)
and io_here =
  | IoHere_DLess_HereEnd of here_end' * word' ref
  | IoHere_DLessDash_HereEnd of here_end' * word' ref

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

and word = Word of string * word_cst

and word_cst = word_component list

and word_component =
  | WordSubshell of subshell_kind * program located
  | WordName of string
  | WordAssignmentWord of assignment_word
  | WordDoubleQuoted of word
  | WordSingleQuoted of word
  | WordLiteral of string
  | WordVariable of variable
  | WordGlobAll
  | WordGlobAny
  | WordReBracketExpression of bracket_expression
  | WordOther
  (* Empty CST. Useful to represent the absence of relevant CSTs. *)
  | WordEmpty

and bracket_expression =
  | BracketExpression_LBRACKET_MatchingList_RBRACKET of matching_list
  | BracketExpression_LBRACKET_NonMatchingList_RBRACKET of nonmatching_list

and matching_list =
  | MatchingList_BracketList of bracket_list

and nonmatching_list =
  | NonMatchingList_BracketList of bracket_list

and bracket_list =
  | BracketList_FollowList of follow_list
  | BracketList_FollowList_MINUS of follow_list

and follow_list =
  | FollowList_ExpressionTerm of expression_term
  | FollowList_FollowList_ExpressionTerm of follow_list * expression_term

and expression_term =
  | ExpressionTerm_SingleExpression of single_expression
  | ExpressionTerm_RangeExpression of range_expression

and single_expression =
  | SingleExpression_EndRange of end_range
  | SingleExpression_CharacterClass of character_class
  | SingleExpression_EquivalenceClass of equivalence_class

and range_expression =
  | RangeExpression_StartRange_EndRange of start_range * end_range
  | RangeExpression_StartRange_MINUS of start_range

and start_range =
  | StartRange_EndRange_MINUS of end_range

and end_range =
  | EndRange_COLLELEMSINGLE of char
  | EndRangeCollatingSymbol of collating_symbol

and collating_symbol =
  | CollatingSymbol_OpenDot_COLLELEMSINGLE_DotClose of char
  | CollatingSymbol_OpenDot_COLLELEMMULTI_DotClose of string
  | CollatingSymbol_OpenDot_METACHAR_DotClose of char

and equivalence_class =
  | EquivalenceClass_OpenEqual_COLLELEMSINGLE_EqualClose of char
  | EquivalenceClass_OpenEqual_COLLELEMMULTI_EqualClose of string

and character_class =
  | CharacterClass_OpenColon_CLASSNAME_ColonClose of class_name

and class_name =
  | ClassName of string (* Keyword in the current LC_CTYPE category. *)

and character_range =
  | Range of char list

and variable =
  | VariableAtom of string * variable_attribute

and variable_attribute =
  | NoAttribute
  | UseDefaultValues of word
  | AssignDefaultValues of word
  | IndicateErrorifNullorUnset of word
  | UseAlternativeValue of word
  | RemoveSmallestSuffixPattern of word
  | RemoveLargestSuffixPattern of word
  | RemoveSmallestPrefixPattern of word
  | RemoveLargestPrefixPattern of word

and subshell_kind =
  | SubShellKindBackQuote
  | SubShellKindParentheses

and name = Name of string

and assignment_word = name * word

and io_number = IONumber of string

and program' = program located
and complete_commands' = complete_commands located
and complete_command' = complete_command located
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
and assignment_word' = assignment_word located

[@@deriving
   yojson,
   visitors { variety = "iter";       ancestors=["located_iter"];      nude=true },
   visitors { variety = "map";        ancestors=["located_map"];       nude=true },
   visitors { variety = "reduce";     ancestors=["located_reduce"];    nude=true },
   visitors { variety = "mapreduce";  ancestors=["located_mapreduce"]; nude=true },
   visitors { variety = "iter2";      ancestors=["located_iter2"];     nude=true },
   visitors { variety = "map2";       ancestors=["located_map2"];      nude=true },
   visitors { variety = "reduce2";    ancestors=["located_reduce2"];   nude=true }
]
