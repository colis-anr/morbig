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

let make level lexbuf =
  let pretokenizer = Prelexer.token level [] in

  (**
      The pretokenizer may produce several pretokens, we use an
      intermediate queue to synchronize pretokens' consumption with
      their production.
  *)
  let q = Queue.create () in
  let push x = Queue.push x q in
    let rec aux () =
      try
        Queue.take q
      with Queue.Empty ->
        List.iter (fun x -> Queue.push x q) (pretokenizer lexbuf);
        aux ()
    in
    aux, push
