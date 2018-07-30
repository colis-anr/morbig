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

(** [save_as_json simplified oc cst] writes the concrete syntax tree [cst]
    to the out_channel [oc]. If [simplified] is [true] then location
    information is omitted, otherwise it is included in the json output. *)
val save_as_json: bool -> out_channel -> CST.complete_command_list -> unit


(** [save_as_dot oc cst] writes the concrete syntax tree [cst]
    to the out_channel [oc] using the DOT format. *)
val save_as_dot: out_channel -> CST.complete_command_list -> unit
