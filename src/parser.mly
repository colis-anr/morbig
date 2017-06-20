(** -*- tuareg -*-

/**************************************************************************/
/*  Copyright (C) 2017 Yann Régis-Gianas, Nicolas Jeannerod,              */
/*  Ralf Treinen.                                                         */
/*                                                                        */
/*  This is free software: you can redistribute it and/or modify it       */
/*  under the terms of the GNU General Public License, version 3.         */
/*  The complete license terms can be found in the file COPYING.          */
/**************************************************************************/

   This grammar specification is almost a verbatim copy of the one of
   the official specification:

             http://pubs.opengroup.org/onlinepubs/9699919799/

   Changes with respect to the specification:

   - There are semantic actions producing concrete syntax trees, of which
     the type definitiosn are in the module {!CST}.

   - Extra tokens have been introduced to denote character-level lexemes.

   - EOF is introduced.

   - The nonterminal 'list' is renamed to 'clist' to avoid a clash with the
     menhir standard library.

   - The terminal 'WORD' is replaced by a non terminal 'word'.

   The official grammar contains the following 7 shift/reduce conflicts:

   Conflict 1:

     Consider "case x in 1) foo
			    bar
	       esac"

     After reading "case x in 1) foo", when we encounter the new line, is
     it (a) to start a new case_item_ns or (b) to continue with the same case
    _item?

     We choose (b).

   Conflict 2:

     Consider ""

     After reading "", when we encounter "", is to (a) ... or (b) ...?

     We choose ...

   Conflict 3:

     Consider ""

     After reading "", when we encounter "", is to (a) ... or (b) ...?

     We choose ...

   Conflict 4:

     Consider ""

     After reading "", when we encounter "", is to (a) ... or (b) ...?

     We choose ...

   Conflict 5:

     Consider ""

     After reading "", when we encounter "", is to (a) ... or (b) ...?

     We choose ...

   Conflict 6:

     Consider ""

     After reading "", when we encounter "", is to (a) ... or (b) ...?

     We choose ...

   Conflict 7:

     Consider ""

     After reading "", when we encounter "", is to (a) ... or (b) ...?

     We choose ...


*)

%{

  open CST

  type here_document_content =
      CST.word ref

%}

(* -------------------------------------------------------
   The grammar symbols
   ------------------------------------------------------- *)
%token<CST.word>  WORD
%token<CST.assignment_word>  ASSIGNMENT_WORD
%token<CST.name>  NAME
%token NEWLINE
%token<CST.io_number>  IO_NUMBER

(* The following are the operators mentioned above. *)


%token  AND_IF    OR_IF    DSEMI
(*      '&&'      '||'     ';;'    *)

%token<CST.word' ref> DLESS DLESSDASH
(*                              '<<'  '<<-' *)

%token  DGREAT  LESSAND  GREATAND  LESSGREAT
(*      '>>'    '<&'     '>&'      '<>' *)


%token  CLOBBER
(*      '>|'   *)


(* The following are the reserved words. *)


%token  If    Then    Else    Elif    Fi    Do    Done
(*      'if'  'then'  'else'  'elif'  'fi'  'do'  'done'   *)


%token  Case    Esac    While    Until    For
(*      'case'  'esac'  'while'  'until'  'for'   *)


(* These are reserved words, not operator tokens, and are
   recognized when reserved words are recognized. *)


%token  Lbrace    Rbrace    Bang
(*      '{'       '}'       '!'   *)


%token  In
(*      'in'   *)

(*changes: Extra token for single-character lexemes. *)
%token  Pipe       Lparen   Rparen   LESS   GREAT  Uppersand  Semicolon
(*      '|'        '('      ')'      '<'    '>'   '&'         ';'       *)


(*changes: Extra standard tokens. *)
%token EOF

(*changes: Conflicts resolution via precedence directives. *)

%left compound_list_newline_list_prec separator_newline_list_prec
      linebreak_empty_prec
%left NEWLINE


(* -------------------------------------------------------
   The Grammar
   ------------------------------------------------------- *)
%start<CST.complete_command>  complete_command
%%
complete_command : l=located(clist) s=located(separator) EOF {
  CompleteCommand_CList_Separator (l, s)
}
| l=located(clist) EOF {
  CompleteCommand_CList l
}
;
clist:
  l=located(clist) s=located(separator_op) a=located(and_or) {
  CList_CList_SeparatorOp_AndOr (l, s, a)
}
| a=located(and_or) {
  CList_AndOr a
}
;
and_or: p=located(pipeline) {
  AndOr_Pipeline p
}
| a=located(and_or) AND_IF l=located(linebreak) p=located(pipeline) {
  AndOr_AndOr_AndIf_LineBreak_Pipeline (a, l , p)
}
| a=located(and_or) OR_IF  l=located(linebreak) p=located(pipeline) {
  AndOr_AndOr_OrIf_LineBreak_Pipeline (a, l, p)
}
;
pipeline: p=located(pipe_sequence) {
  Pipeline_PipeSequence p
}
| Bang p=located(pipe_sequence) {
  Pipeline_Bang_PipeSequence p
}
;
pipe_sequence:
  c=located(command) {
  PipeSequence_Command c
}
| p=located(pipe_sequence) Pipe l=located(linebreak) c=located(command) {
  PipeSequence_PipeSequence_Pipe_LineBreak_Command (p, l, c)
}
;
command:
  s=located(simple_command) {
  Command_SimpleCommand s
}
| c=located(compound_command) {
  Command_CompoundCommand c
}
| c=located(compound_command) r=located(redirect_list) {
  Command_CompoundCommand_RedirectList (c, r)
}
| f=located(function_definition) {
  Command_FunctionDefinition f
}
;
compound_command : b=located(brace_group) {
  CompoundCommand_BraceGroup b
}
| s=located(subshell) {
  CompoundCommand_Subshell s
}
| f=located(for_clause) {
  CompoundCommand_ForClause f
}
| c=located(case_clause) {
  CompoundCommand_CaseClause c
}
| i=located(if_clause) {
  CompoundCommand_IfClause i
}
| w=located(while_clause) {
  CompoundCommand_WhileClause w
}
| u=located(until_clause) {
  CompoundCommand_UntilClause u
}
;
subshell         : Lparen c=located(compound_list) Rparen {
  Subshell_Lparen_CompoundList_Rparen c
}
;
compound_list    : t=located(term) %prec compound_list_newline_list_prec {
  CompoundList_Term t
}
| n=located(newline_list) t=located(term)
%prec compound_list_newline_list_prec {
  CompoundList_NewLineList_Term (n, t)
}
| t=located(term) s=located(separator) {
  CompoundList_Term_Separator (t, s)
}
| n=located(newline_list) t=located(term) s=located(separator) {
  CompoundList_NewLineList_Term_Separator (n, t, s)
}
;
term             : t=located(term) s=located(separator) a=located(and_or) {
  Term_Term_Separator_AndOr (t, s, a)
}
|                a=located(and_or) {
  Term_AndOr a
}
;
for_clause: For n=located(name) d=located(do_group) {
  ForClause_For_Name_DoGroup (n, d)
}
| For n=located(name) s=located(sequential_sep) d=located(do_group) {
  ForClause_For_Name_SequentialSep_DoGroup (n, s, d)
}
| For n=located(name)
  l=located(linebreak) cin s=located(sequential_sep) d=located(do_group) {
  ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (n, l, s, d)
}
| For n=located(name)
  l=located(linebreak) cin
  w=located(wordlist) s=located(sequential_sep) d=located(do_group) {
  ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (n, l, w, s, d)
}
;
name: n=NAME                    /* Apply rule 5 */ {
  n
}
;
cin              : In                       /* Apply rule 6 */ {
  ()
}
;
wordlist         : wl=located(wordlist) w=located(word) {
  WordList_WordList_Word (wl, w)
}
|          w=located(word) {
  WordList_Word w
}
;
case_clause: Case w=located(word) l1=located(linebreak) cin l2=located(linebreak) c=located(case_list) Esac {
  CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (w, l1, l2, c)
}
| Case w=located(word) l1=located(linebreak) cin l2=located(linebreak) c=located(case_list_ns) Esac {
  CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (w, l1, l2, c)
}
| Case w=located(word) l1=located(linebreak) cin l2=located(linebreak) Esac {
  CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (w, l1, l2)
}
;
case_list_ns     : c=located(case_list) ci=located(case_item_ns) {
  CaseListNS_CaseList_CaseItemNS (c, ci)
}
|           ci=located(case_item_ns) {
  CaseListNS_CaseItemNS ci
}
;
case_list        : c=located(case_list) ci=located(case_item) {
  CaseList_CaseList_CaseItem (c, ci)
}
|           ci=located(case_item) {
  CaseList_CaseItem ci
}
;
case_item_ns     : p=located(pattern) Rparen l=located(linebreak) {
  CaseItemNS_Pattern_Rparen_LineBreak (p, l)
}
| p=located(pattern) Rparen c=located(compound_list) l=located(linebreak) {
  CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (p, c, l)
}
| Lparen p=located(pattern) Rparen l=located(linebreak) {
  CaseItemNS_Lparen_Pattern_Rparen_LineBreak (p, l)
}
| Lparen p=located(pattern) Rparen c=located(compound_list) l=located(linebreak) {
  CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (p, c, l)
}
;
case_item        : p=located(pattern) Rparen l1=located(linebreak) DSEMI l2=located(linebreak) {
  CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2)
}
| p=located(pattern) Rparen c=located(compound_list) DSEMI l=located(linebreak) {
  CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l)
}
| Lparen p=located(pattern) Rparen l1=located(linebreak) DSEMI l2=located(linebreak) {
  CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2)
}
| Lparen p=located(pattern) Rparen c=located(compound_list) DSEMI l=located(linebreak) {
  CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l)
}
;
pattern          : w=located(word)       /* Apply rule 4 */ {
  Pattern_Word w
}
| p=located(pattern) Pipe w=located(word)         /* Do not apply rule 4 */ {
  Pattern_Pattern_Pipe_Word (p, w)
}
;
if_clause        : If c1=located(compound_list) Then c2=located(compound_list) e=located(else_part) Fi {
  IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (c1, c2, e)
}
| If c1=located(compound_list) Then c2=located(compound_list) Fi {
  IfClause_If_CompoundList_Then_CompoundList_Fi (c1, c2)
}
;
else_part        : Elif c1=located(compound_list) Then c2=located(compound_list) {
  ElsePart_Elif_CompoundList_Then_CompoundList (c1, c2)
}
| Elif c1=located(compound_list) Then c2=located(compound_list) e=located(else_part) {
  ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (c1, c2, e)
}
| Else c=located(compound_list) {
  ElsePart_Else_CompoundList c
}
;
while_clause     : While c=located(compound_list) d=located(do_group) {
  WhileClause_While_CompoundList_DoGroup (c, d)
}
;
until_clause     : Until c=located(compound_list) d=located(do_group) {
  UntilClause_Until_CompoundList_DoGroup (c, d)
}
;
function_definition : f=located(fname) Lparen Rparen l=located(linebreak) fb=located(function_body) {
  FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (f, l, fb)
}
;
function_body    : c=located(compound_command)                /* Apply rule 9 */ {
  FunctionBody_CompoundCommand c
}
| c=located(compound_command) r=located(redirect_list)                /* Apply rule 9 */ {
  FunctionBody_CompoundCommand_RedirectList (c, r)
}
;
fname            : n=NAME                            /* Apply rule 8 */ {
  Fname_Name n
}
;
brace_group      : Lbrace c=located(compound_list) Rbrace {
  BraceGroup_LBrace_CompoundList_RBrace c
}
;
do_group         : Do c=located(compound_list) Done           /* Apply rule 6 */ {
  DoGroup_Do_CompoundList_Done c
}
;
simple_command   : cp=located(cmd_prefix) cw=located(cmd_word) cs=located(cmd_suffix) {
  SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, cs)
}
| cp=located(cmd_prefix) cw=located(cmd_word) {
  SimpleCommand_CmdPrefix_CmdWord (cp, cw)
}
| cp=located(cmd_prefix) {
  SimpleCommand_CmdPrefix cp
}
| cn=located(cmd_name) cs=located(cmd_suffix) {
  SimpleCommand_CmdName_CmdSuffix (cn, cs)
}
| cn=located(cmd_name) {
  SimpleCommand_CmdName cn
}
;
cmd_name         : w=located(word)                   /* Apply rule 7a */ {
  CmdName_Word w
}
;
cmd_word         : w=located(word)                   /* Apply rule 7b */ {
  CmdWord_Word w
}
;
cmd_prefix       : i=located(io_redirect) {
  CmdPrefix_IoRedirect i
}
| cp=located(cmd_prefix) i=located(io_redirect) {
  CmdPrefix_CmdPrefix_IoRedirect (cp, i)
}
| a=located(ASSIGNMENT_WORD) {
  CmdPrefix_AssignmentWord a
}
| cp=located(cmd_prefix) a=located(ASSIGNMENT_WORD) {
  CmdPrefix_CmdPrefix_AssignmentWord (cp, a)
}
;
cmd_suffix       : i=located(io_redirect) {
  CmdSuffix_IoRedirect i
}
| cs=located(cmd_suffix) i=located(io_redirect) {
  CmdSuffix_CmdSuffix_IoRedirect (cs, i)
}
| w=located(word) {
  CmdSuffix_Word w
}
| cs=located(cmd_suffix) w=located(word) {
  CmdSuffix_CmdSuffix_Word (cs, w)
}
;
redirect_list    : i=located(io_redirect) {
  RedirectList_IoRedirect i
}
| r=located(redirect_list) i=located(io_redirect) {
  RedirectList_RedirectList_IoRedirect (r, i)
}
;
io_redirect      : i=located(io_file) {
  IoRedirect_IoFile i
}
| ion=IO_NUMBER i=located(io_file) {
  IoRedirect_IoNumber_IoFile (ion, i)
}
| ioh=located(io_here) {
  IoRedirect_IoHere ioh
}
| ion=IO_NUMBER ioh=located(io_here) {
  IoRedirect_IoNumber_IoHere (ion, ioh)
}
;
io_file          : LESS f=located(filename) {
  IoFile_Less_FileName f
}
| LESSAND   f=located(filename) {
  IoFile_LessAnd_FileName f
}
| GREAT     f=located(filename) {
  IoFile_Great_FileName f
}
| GREATAND  f=located(filename) {
  IoFile_GreatAnd_FileName f
}
| DGREAT    f=located(filename) {
  IoFile_DGreat_FileName f
}
| LESSGREAT f=located(filename) {
 IoFile_LessGreat_FileName f
}
| CLOBBER   f=located(filename) {
  IoFile_Clobber_FileName f
}
;
filename         : w=located(word)                      /* Apply rule 2 */ {
  Filename_Word w
}
;
io_here          : heredocument_placeholder=DLESS he=located(here_end) {
  IoHere_DLess_HereEnd (he, heredocument_placeholder)
}
| heredocument_placeholder=DLESSDASH he=located(here_end) {
  IoHere_DLessDash_HereEnd (he, heredocument_placeholder)
}
;
here_end         : w=located(word)                      /* Apply rule 3 */ {
  HereEnd_Word w
}
;
newline_list:
  NEWLINE {
  NewLineList_NewLine
}
| l=located(newline_list) NEWLINE {
  NewLineList_NewLineList_NewLine l
}
;
linebreak: n=located(newline_list) %prec linebreak_empty_prec {
  LineBreak_NewLineList n
}
| /* empty */ %prec linebreak_empty_prec {
  LineBreak_Empty
}
;
separator_op : Uppersand {
  SeparatorOp_Uppersand
}
| Semicolon {
  SeparatorOp_Semicolon
}
;
separator : s=located(separator_op) l=located(linebreak) {
  Separator_SeparatorOp_LineBreak (s, l)
}
| n=located(newline_list) %prec separator_newline_list_prec {
  Separator_NewLineList n
}
;
sequential_sep : Semicolon l=located(linebreak) {
  SequentialSep_Semicolon_LineBreak l
}
| n=located(newline_list) {
  SequentialSep_NewLineList n
}

%inline word: w=WORD {
  w
}
| n=NAME {
  CST.word_of_name n
}

%inline located(X): x=X {
  with_poss $startpos $endpos x
}
