(**

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

  open SemanticValues
  open CST

  type here_document_content =
      SemanticValues.word ref

%}

(* -------------------------------------------------------
   The grammar symbols
   ------------------------------------------------------- *)
%token<SemanticValues.word>  WORD
%token<SemanticValues.word SemanticValues.assignment_word>  ASSIGNMENT_WORD
%token<SemanticValues.name>  NAME
%token NEWLINE
%token<SemanticValues.io_number>  IO_NUMBER

(* The following are the operators mentioned above. *)


%token  AND_IF    OR_IF    DSEMI
(*      '&&'      '||'     ';;'    *)

%token<SemanticValues.word ref> DLESS DLESSDASH
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
complete_command : l=clist s=separator EOF {
  CompleteCommand_CList_Separator (l, s)
}
| l=clist EOF {
  CompleteCommand_CList l
}
;
clist:
  l=clist s=separator_op a=and_or {
  CList_CList_SeparatorOp_AndOr (l, s, a)
}
| a=and_or {
  CList_AndOr a
}
;
and_or: p=pipeline {
  AndOr_Pipeline p
}
| a=and_or AND_IF l=linebreak p=pipeline {
  AndOr_AndOr_AndIf_LineBreak_Pipeline (a, l , p)
}
| a=and_or OR_IF  l=linebreak p=pipeline {
  AndOr_AndOr_OrIf_LineBreak_Pipeline (a, l, p)
}
;
pipeline: p=pipe_sequence {
  Pipeline_PipeSequence p
}
| Bang p=pipe_sequence {
  Pipeline_Bang_PipeSequence p
}
;
pipe_sequence:
  c=command {
  PipeSequence_Command c
}
| p=pipe_sequence Pipe l=linebreak c=command {
  PipeSequence_PipeSequence_Pipe_LineBreak_Command (p, l, c)
}
;
command:
  s=simple_command {
  Command_SimpleCommand s
}
| c=compound_command {
  Command_CompoundCommand c
}
| c=compound_command r=redirect_list {
  Command_CompoundCommand_RedirectList (c, r)
}
| f=function_definition {
  Command_FunctionDefinition f
}
;
compound_command : b=brace_group {
  CompoundCommand_BraceGroup b
}
| s=subshell {
  CompoundCommand_Subshell s
}
| f=for_clause {
  CompoundCommand_ForClause f
}
| c=case_clause {
  CompoundCommand_CaseClause c
}
| i=if_clause {
  CompoundCommand_IfClause i
}
| w=while_clause {
  CompoundCommand_WhileClause w
}
| u=until_clause {
  CompoundCommand_UntilClause u
}
;
subshell         : Lparen c=compound_list Rparen {
  Subshell_Lparen_CompoundList_Rparen c
}
;
compound_list    : t=term %prec compound_list_newline_list_prec {
  CompoundList_Term t
}
| n=newline_list t=term %prec compound_list_newline_list_prec {
  CompoundList_NewLineList_Term (n, t)
}
| t=term s=separator {
  CompoundList_Term_Separator (t, s)
}
| n=newline_list t=term s=separator {
  CompoundList_NewLineList_Term_Separator (n, t, s)
}
;
term             : t=term s=separator a=and_or {
  Term_Term_Separator_AndOr (t, s, a)
}
|                a=and_or {
  Term_AndOr a
}
;
for_clause       : For n=name l=linebreak d=do_group {
  ForClause_For_Name_LineBreak_DoGroup (n, l, d)
}
| For n=name l=linebreak cin s=sequential_sep d=do_group {
  ForClause_For_Name_LineBreak_In_SequentialSep_DoGroup (n, l, s, d)
}
| For n=name l=linebreak cin w=wordlist s=sequential_sep d=do_group {
  ForClause_For_Name_LineBreak_In_WordList_SequentialSep_DoGroup (n, l, w, s, d)
}
;
name             : n=NAME                    /* Apply rule 5 */ {
  Name n
}
;
cin              : In                       /* Apply rule 6 */ {
  ()
}
;
wordlist         : wl=wordlist w=word {
  WordList_WordList_Word (wl, w)
}
|          w=word {
  WordList_Word w
}
;
case_clause      : Case w=word l1=linebreak cin l2=linebreak c=case_list Esac {
  CaseClause_Case_Word_LineBreak_In_LineBreak_CaseList_Esac (w, l1, l2, c)
}
| Case w=word l1=linebreak cin l2=linebreak c=case_list_ns Esac {
  CaseClause_Case_Word_LineBreak_In_LineBreak_CaseListNS_Esac (w, l1, l2, c)
}
| Case w=word l1=linebreak cin l2=linebreak Esac {
  CaseClause_Case_Word_LineBreak_In_LineBreak_Esac (w, l1, l2)
}
;
case_list_ns     : c=case_list ci=case_item_ns {
  CaseListNS_CaseList_CaseItemNS (c, ci)
}
|           ci=case_item_ns {
  CaseListNS_CaseItemNS ci
}
;
case_list        : c=case_list ci=case_item {
  CaseList_CaseList_CaseItem (c, ci)
}
|           ci=case_item {
  CaseList_CaseItem ci
}
;
case_item_ns     : p=pattern Rparen l=linebreak {
  CaseItemNS_Pattern_Rparen_LineBreak (p, l)
}
| p=pattern Rparen c=compound_list l=linebreak {
  CaseItemNS_Pattern_Rparen_CompoundList_LineBreak (p, c, l)
}
| Lparen p=pattern Rparen l=linebreak {
  CaseItemNS_Lparen_Pattern_Rparen_LineBreak (p, l)
}
| Lparen p=pattern Rparen c=compound_list l=linebreak {
  CaseItemNS_Lparen_Pattern_Rparen_CompoundList_LineBreak (p, c, l)
}
;
case_item        : p=pattern Rparen l1=linebreak DSEMI l2=linebreak {
  CaseItem_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2)
}
| p=pattern Rparen c=compound_list DSEMI l=linebreak {
  CaseItem_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l)
}
| Lparen p=pattern Rparen l1=linebreak DSEMI l2=linebreak {
  CaseItem_Lparen_Pattern_Rparen_LineBreak_Dsemi_LineBreak (p, l1, l2)
}
| Lparen p=pattern Rparen c=compound_list DSEMI l=linebreak {
  CaseItem_Lparen_Pattern_Rparen_CompoundList_Dsemi_LineBreak (p, c, l)
}
;
pattern          : w=word       /* Apply rule 4 */ {
  Pattern_Word w
}
| p=pattern Pipe w=word         /* Do not apply rule 4 */ {
  Pattern_Pattern_Pipe_Word (p, w)
}
;
if_clause        : If c1=compound_list Then c2=compound_list e=else_part Fi {
  IfClause_If_CompoundList_Then_CompoundList_ElsePart_Fi (c1, c2, e)
}
| If c1=compound_list Then c2=compound_list Fi {
  IfClause_If_CompoundList_Then_CompoundList_Fi (c1, c2)
}
;
else_part        : Elif c1=compound_list Then c2=compound_list {
  ElsePart_Elif_CompoundList_Then_CompoundList (c1, c2)
}
| Elif c1=compound_list Then c2=compound_list e=else_part {
  ElsePart_Elif_CompoundList_Then_CompoundList_ElsePart (c1, c2, e)
}
| Else c=compound_list {
  ElsePart_Else_CompoundList c
}
;
while_clause     : While c=compound_list d=do_group{
  WhileClause_While_CompoundList_DoGroup (c, d)
}
;
until_clause     : Until c=compound_list d=do_group {
  UntilClause_Until_CompoundList_DoGroup (c, d)
}
;
function_definition : f=fname Lparen Rparen l=linebreak fb=function_body {
  FunctionDefinition_Fname_Lparen_Rparen_LineBreak_FunctionBody (f, l, fb)
}
;
function_body    : c=compound_command                /* Apply rule 9 */ {
  FunctionBody_CompoundCommand c
}
| c=compound_command r=redirect_list                /* Apply rule 9 */ {
  FunctionBody_CompoundCommand_RedirectList (c, r)
}
;
fname            : n=NAME                            /* Apply rule 8 */ {
  Fname_Name n
}
;
brace_group      : Lbrace c=compound_list Rbrace {
  BraceGroup_LBrace_CompoundList_RBrace c
}
;
do_group         : Do c=compound_list Done           /* Apply rule 6 */ {
  DoGroup_Do_CompoundList_Done c
}
;
simple_command   : cp=cmd_prefix cw=cmd_word cs=cmd_suffix {
  SimpleCommand_CmdPrefix_CmdWord_CmdSuffix (cp, cw, cs)
}
| cp=cmd_prefix cw=cmd_word {
  SimpleCommand_CmdPrefix_CmdWord (cp, cw)
}
| cp=cmd_prefix {
  SimpleCommand_CmdPrefix cp
}
| cn=cmd_name cs=cmd_suffix {
  SimpleCommand_CmdName_CmdSuffix (cn, cs)
}
| cn=cmd_name {
  SimpleCommand_CmdName cn
}
;
cmd_name         : w=word                   /* Apply rule 7a */ {
  CmdName_Word w
}
;
cmd_word         : w=word                   /* Apply rule 7b */ {
  CmdWord_Word w
}
;
cmd_prefix       : i=io_redirect {
  CmdPrefix_IoRedirect i
}
| cp=cmd_prefix i=io_redirect {
  CmdPrefix_CmdPrefix_IoRedirect (cp, i)
}
| a=ASSIGNMENT_WORD {
  CmdPrefix_AssignmentWord a
}
| cp=cmd_prefix a=ASSIGNMENT_WORD {
  CmdPrefix_CmdPrefix_AssignmentWord (cp, a)
}
;
cmd_suffix       : i=io_redirect {
  CmdSuffix_IoRedirect i
}
| cs=cmd_suffix i=io_redirect {
  CmdSuffix_CmdSuffix_IoRedirect (cs, i)
}
| w=word {
  CmdSuffix_Word w
}
| cs=cmd_suffix w=word {
  CmdSuffix_CmdSuffix_Word (cs, w)
}
;
redirect_list    : i=io_redirect {
  RedirectList_IoRedirect i
}
| r=redirect_list i=io_redirect {
  RedirectList_RedirectList_IoRedirect (r, i)
}
;
io_redirect      : i=io_file {
  IoRedirect_IoFile i
}
| ion=IO_NUMBER i=io_file {
  IoRedirect_IoNumber_IoFile (ion, i)
}
| ioh=io_here {
  IoRedirect_IoHere ioh
}
| ion=IO_NUMBER ioh=io_here {
  IoRedirect_IoNumber_IoHere (ion, ioh)
}
;
io_file          : LESS f=filename {
  IoFile_Less_FileName f
}
| LESSAND   f=filename {
  IoFile_LessAnd_FileName f
}
| GREAT     f=filename {
  IoFile_Great_FileName f
}
| GREATAND  f=filename {
  IoFile_GreatAnd_FileName f
}
| DGREAT    f=filename {
  IoFile_DGreat_FileName f
}
| LESSGREAT f=filename {
 IoFile_LessGreat_FileName f
}
| CLOBBER   f=filename {
  IoFile_Clobber_FileName f
}
;
filename         : w=word                      /* Apply rule 2 */ {
  Filename_Word w
}
;
io_here          : DLESS he=here_end {
  IoHere_DLess_HereEnd he
}
| DLESSDASH he=here_end {
  IoHere_DLessDash_HereEnd he
}
;
here_end         : w=word                      /* Apply rule 3 */ {
  HereEnd_Word w
}
;
newline_list:
  NEWLINE {
  NewLineList_NewLine
}
| l=newline_list NEWLINE {
  NewLineList_NewLineList_NewLine l
}
;
linebreak: n=newline_list %prec linebreak_empty_prec {
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
separator : s=separator_op l=linebreak {
  Separator_SeparatorOp_LineBreak (s, l)
}
| n=newline_list %prec separator_newline_list_prec {
  Separator_NewLineList n
}
;
sequential_sep : Semicolon l=linebreak {
  SequentialSep_Semicolon_LineBreak l
}
| n=newline_list {
  SequentialSep_NewLineList n
}

%inline word: w=WORD {
  w
}
| n=NAME {
  SemanticValues.word_of_name n
}

