/**  -*- tuareg -*- *******************************************************/
/*                                                                        */
/*  Copyright (C) 2017,2018 Yann Régis-Gianas, Nicolas Jeannerod,         */
/*  Ralf Treinen.                                                         */
/*                                                                        */
/*  This is free software: you can redistribute it and/or modify it       */
/*  under the terms of the GNU General Public License, version 3.         */
/*                                                                        */
/*  Additional terms apply, due to the reproduction of portions of        */
/*  the POSIX standard. Please refer to the file COPYING for details.     */
/**************************************************************************/

%{
   open CST
%}

/*

   This grammar is almost the same as the one of the standard. It differs
   simply in the treatment of META_CHAR, which is nonterminal instead of
   a terminal since it is the union of MINUS, HAT and RBRACKET.
   See {!PatternMatchingRecognizer.next_token} for further explanations
   about this change.

   In addition, Menhir detects a shift/reduce conflict in this grammar.
   Indeed, when the parser considers:

         [ end_range . MINUS ]

   It does not know if it should reduce to build the intermediate sentence:

         [ single_expression . MINUS ]

   (So that it will be further recognized as [ follow_list . MINUS ])

   or if it should shift MINUS to build the intermediate sentence:

         [ single_expression . MINUS ]

   (So that it will be further rewritten as [ start_range end_range ]
    and then as a [ follow_list ] (without the minus).)

   The two interpretations seem semantically equivalent: hence, we
   make an arbitrary choice here, which is "shift", as it is the usual
   arbitrary choice made by LR(1) parsers when solving conflicts
   automatically.

*/

%token MINUS HAT LBRACKET RBRACKET
%token<char> COLL_ELEM_SINGLE /* META_CHAR */
%token<string> COLL_ELEM_MULTI

%token EOF

%token    Open_equal Equal_close Open_dot Dot_close Open_colon Colon_close
/*           '[='       '=]'        '[.'     '.]'      '[:'       ':]'  */

%nonassoc reduce_end_range_prec
%nonassoc MINUS

%start<CST.bracket_expression>    bracket_expression
%%

/* --------------------------------------------
   Bracket Expression
   -------------------------------------------
*/
bracket_expression : LBRACKET m=matching_list RBRACKET EOF
{
  BracketExpression_LBRACKET_MatchingList_RBRACKET m
}
| LBRACKET n=nonmatching_list RBRACKET EOF
{
  BracketExpression_LBRACKET_NonMatchingList_RBRACKET n
}
;
matching_list  : b=bracket_list
{
  MatchingList_BracketList b
}
;
nonmatching_list : HAT b=bracket_list
{
  NonMatchingList_BracketList b
}
;
bracket_list   : f=follow_list
{
  BracketList_FollowList f
}
| f=follow_list MINUS
{
  BracketList_FollowList_MINUS f
}
;
follow_list    :             e=expression_term
{
  FollowList_ExpressionTerm e
}
| f=follow_list e=expression_term
{
  FollowList_FollowList_ExpressionTerm (f, e)
}
;
expression_term : s=single_expression
{
  ExpressionTerm_SingleExpression s
}
| r=range_expression
{
  ExpressionTerm_RangeExpression r
}
;
single_expression : e=end_range %prec reduce_end_range_prec
{
  SingleExpression_EndRange e
}
| c=character_class
{
  SingleExpression_CharacterClass c
}
| c=equivalence_class
{
  SingleExpression_EquivalenceClass c
}
;
range_expression : s=start_range e=end_range
{
  RangeExpression_StartRange_EndRange (s, e)
}
| s=start_range MINUS
{
  RangeExpression_StartRange_MINUS s
}
;
start_range    : e=end_range MINUS
{
  StartRange_EndRange_MINUS e
}
;
end_range      : c=COLL_ELEM_SINGLE
{
  EndRange_COLLELEMSINGLE c
}
| c=collating_symbol
{
  EndRangeCollatingSymbol c
}
;
collating_symbol :
  Open_dot c=COLL_ELEM_SINGLE Dot_close
{
  CollatingSymbol_OpenDot_COLLELEMSINGLE_DotClose c
}
| Open_dot s=COLL_ELEM_MULTI Dot_close
{
  CollatingSymbol_OpenDot_COLLELEMMULTI_DotClose s
}
| Open_dot m=meta_char Dot_close
{
  CollatingSymbol_OpenDot_METACHAR_DotClose m
}
;
equivalence_class : Open_equal c=COLL_ELEM_SINGLE Equal_close
{
  EquivalenceClass_OpenEqual_COLLELEMSINGLE_EqualClose c
}
| Open_equal s=COLL_ELEM_MULTI Equal_close
{
  EquivalenceClass_OpenEqual_COLLELEMMULTI_EqualClose s
}
;

character_class : Open_colon s=class_name Colon_close
{
  CharacterClass_OpenColon_CLASSNAME_ColonClose (ClassName s)
}
;

/* The following rule is not in the grammar of the POSIX standard as
   explained in the header of this file. */
meta_char: MINUS {
  '-'
}
| HAT {
  '^'
}
| RBRACKET {
  ']'
}
;

class_name: s=COLL_ELEM_SINGLE+
{
  let b = Buffer.create 13 in
  List.iter (Buffer.add_char b) s;
  let s = Buffer.contents b in
  if Name.is_name s then s else raise Parsing.Parse_error
}
