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
open CST
open Names
open Keywords

(**specification

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

let recognize_assignment checkpoint pretoken w = FirstSuccessMonad.(
  match Str.(split_delim (regexp "=") w) with
    | [w] ->
      fail
    | [""; w] ->
      return (WORD (CST.Word ("=" ^ w)))
    | name :: rhs ->
      let rhs = String.concat "=" rhs in
      if is_name name then
        let aword = CST.(AssignmentWord (Name name, Word rhs)) in
        let (_, pstart, pstop) = pretoken in
        let token = ASSIGNMENT_WORD aword in
        if accepted_token checkpoint (token, pstart, pstop) <> Wrong then
          return token
        else
          return (WORD (CST.Word w))
      else
        (* We choose to return a WORD. *)
        return (WORD (Word w))
    | _ ->
      return (WORD (Word w))
)
