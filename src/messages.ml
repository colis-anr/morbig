(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018 Yann Régis-Gianas, Nicolas Jeannerod,         *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

let message what consequence ?filename msg =
  Printf.eprintf "%s[%s] %s\n"
    (match filename with None -> "" | Some f -> f ^":0:0:\n")
    what
    msg;
  consequence ()

let warning = message "WARNING" ignore
let error = message "ERROR" (fun () -> exit 1)
