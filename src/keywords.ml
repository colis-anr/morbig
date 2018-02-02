(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017 Yann Régis-Gianas, Nicolas Jeannerod,             *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

open ExtPervasives
open ExtMenhirLib
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open MenhirLib.General

(**specification

   /* The following are the reserved words. */


   %token  If    Then    Else    Elif    Fi    Do    Done
   /*      'if'  'then'  'else'  'elif'  'fi'  'do'  'done'   */


   %token  Case    Esac    While    Until    For
   /*      'case'  'esac'  'while'  'until'  'for'   */

   /* These are reserved words, not operator tokens, and are
      recognized when reserved words are recognized. */


   %token  Lbrace    Rbrace    Bang
   /*      '{'       '}'       '!'   */


   %token  In
   /*      'in'   */

*)
let keywords = [
    "if",    If,     X (T T_If);
    "then",  Then,   X (T T_Then);
    "else",  Else,   X (T T_Else);
    "elif",  Elif,   X (T T_Elif);
    "fi",    Fi,     X (T T_Fi);
    "do",    Do,     X (T T_Do);
    "done",  Done,   X (T T_Done);
    "case",  Case,   X (T T_Case);
    "esac",  Esac,   X (T T_Esac);
    "while", While,  X (T T_While);
    "until", Until,  X (T T_Until);
    "for",   For,    X (T T_For);
    "{",     Lbrace, X (T T_Lbrace);
    "}",     Rbrace, X (T T_Rbrace);
    "!",     Bang,   X (T T_Bang);
    "in",    In,     X (T T_In);
]

let keyword_of_string =
  let t = Hashtbl.create 13 in
  List.iter (fun (s, kwd, _) -> Hashtbl.add t s kwd) keywords;
  Hashtbl.find t

let is_reserved_word w =
  try ignore (keyword_of_string w); true with _ -> false

let terminal_of_keyword k =
  let (_, _, t) = List.find (fun (_, k', _) -> k = k') keywords in
  t

let recognize_reserved_word_if_relevant checkpoint (pretoken, pstart, pstop) w =
  FirstSuccessMonad.(
    try
      let kwd = keyword_of_string w in
      if accepted_token checkpoint (kwd, pstart, pstop) <> Wrong then
        return kwd
      else
        raise Not_found
    with Not_found ->
      if Names.is_name w then
        return (NAME (CST.Name w))
      else
        return (WORD (CST.Word w))
  )
