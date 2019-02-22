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


[@@deriving
   yojson,
   visitors { variety = "iter";      name = "located_iter";      polymorphic = true },
   visitors { variety = "map";       name = "located_map";       polymorphic = true },
   visitors { variety = "reduce";    name = "located_reduce";    polymorphic = true },
   visitors { variety = "mapreduce"; name = "located_mapreduce"; polymorphic = true },
   visitors { variety = "iter2";     name = "located_iter2";     polymorphic = true },
   visitors { variety = "map2";      name = "located_map2";      polymorphic = true },
   visitors { variety = "reduce2";   name = "located_reduce2";   polymorphic = true }
]
