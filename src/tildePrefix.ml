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

(*specification

  A "tilde-prefix" consists of an unquoted <tilde> character at the
   beginning of a word, followed by all of the characters preceding
   the first unquoted <slash> in the word, or all the characters in
   the word if there is no <slash>. In an assignment (see XBD Variable
   Assignment), multiple tilde-prefixes can be used: at the beginning
   of the word (that is, following the <equals-sign> of the
   assignment), following any unquoted <colon>, or both. A
   tilde-prefix in an assignment is terminated by the first unquoted
   <colon> or <slash>. If none of the characters in the tilde-prefix
   are quoted, the characters in the tilde-prefix following the
   <tilde> are treated as a possible login name from the user
   database. A portable login name cannot contain characters outside
   the set given in the description of the LOGNAME environment
   variable in XBD Other Environment Variables. If the login name is
   null (that is, the tilde-prefix contains only the tilde), the
   tilde-prefix is replaced by the value of the variable HOME. If HOME
   is unset, the results are unspecified. Otherwise, the tilde-prefix
   shall be replaced by a pathname of the initial working directory
   associated with the login name obtained using the getpwnam()
   function as defined in the System Interfaces volume of
   POSIX.1-2017. If the system does not recognize the login name, the
   results are undefined.

*)
open CST

let find_login s =
  match Str.(split (regexp "/") s) with
  | login :: rem -> [WordTildePrefix login; WordLiteral (String.concat "/" rem)]
  | _ -> assert false (* Because there is slash, or not. *)

let rec make_tilde_prefix_explicit rhs_assignment = function
  | (WordLiteral s) as cst when s <> "" ->
     if s.[0] = '~' then (
       if rhs_assignment then
         let s = Str.(split (regexp ":") s) in
         List.(flatten (map find_login s))
       else
         find_login s
     ) else [cst]
  | WordAssignmentWord (name, Word (s, csts)) ->
     let csts = recognize ~rhs_assignment:true csts in
     [WordAssignmentWord (name, Word (s, csts))]
  | cst ->
     [cst]

and recognize ?(rhs_assignment=false) csts =
  List.(flatten (map (make_tilde_prefix_explicit rhs_assignment) csts))
