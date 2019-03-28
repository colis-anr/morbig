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

open ExtPervasives
open ExtMenhirLib
open Parser
open Parser.MenhirInterpreter

(*specification:

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
  fun w -> FirstSuccessMonad.(
    try return (Hashtbl.find t w) with Not_found -> fail
  )

let is_reserved_word w =
  FirstSuccessMonad.run (keyword_of_string w) <> None

let terminal_of_keyword k =
  let (_, _, t) = List.find (fun (_, k', _) -> k = k') keywords in
  t

let must_be_well_delimited flag = function
  | Rbrace | Do | Done | Then | Else | Elif -> flag
  | _ -> true

let recognize_reserved_word_if_relevant well_delimited checkpoint p w =
  let (_, pstart, pstop) = p in
  let valid_token kwd =
    accepted_token checkpoint (kwd, pstart, pstop) <> Wrong
    && must_be_well_delimited well_delimited kwd
  in
  FirstSuccessMonad.(
    let as_keyword =
      keyword_of_string w >>= fun kwd ->
      return_if (valid_token kwd) kwd
    in
    let as_name =
      return_if (Name.is_name w) (NAME (CST.Name w))
    in
    as_keyword +> as_name
  )
