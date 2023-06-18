(**************************************************************************)
(*  Copyright (C) 2017-2023 Yann Régis-Gianas, Nicolas Jeannerod,         *)
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
open CST
open Name

(*specification:

   [Assignment preceding command name]

   [When the first word]

   If the TOKEN does not contain the character '=', rule 1 is
   applied. Otherwise, 7b shall be applied.

   [Not the first word]

   If the TOKEN contains the <equals-sign> character:

   If it begins with '=', the token WORD shall be returned.

   If all the characters preceding '=' form a valid name (see XBD
   Name), the token ASSIGNMENT_WORD shall be returned. (Quoted
   characters cannot participate in forming a valid name.)

   Otherwise, it is unspecified whether it is ASSIGNMENT_WORD or WORD
   that is returned.

   Assignment to the NAME shall occur as specified in Simple Commands.

*)

let recognize_assignment checkpoint pretoken (Word (word_str, word_cst)) =
  FirstSuccessMonad.(
    match word_cst with
    | WordLiteral literal :: word_cst_leftover ->
      (
        match String.index_opt literal '=' with
        | None | Some 0 -> fail
        | Some i ->
          let name = String.sub literal 0 i in
          if is_name name then
            let (_, pstart, pstop) = pretoken in
            let literal_leftover = String.sub literal (i + 1) (String.length literal - i - 1) in
            let word_str_leftover = String.sub word_str (i + 1) (String.length word_str - i - 1) in
            let word_cst =
              if literal_leftover = "" then
                word_cst_leftover
              else
                WordLiteral literal_leftover :: word_cst_leftover
            in
            let word = Word (word_str_leftover, word_cst) in
            let token = ASSIGNMENT_WORD (Name name, word) in
            if accepted_token checkpoint (token, pstart, pstop) <> Wrong then
              return token
            else
              fail
          else
            fail
      )
    | _ -> fail
  )
