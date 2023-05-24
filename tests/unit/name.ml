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

let spf = Format.sprintf

let test_case_is_name ~valid name =
  let text =
    if valid then
      spf "`%s` is a valid name" name
    else
      spf "`%s` is an invalid name" name
  in
  Alcotest.test_case
    text
    `Quick
    (
      fun () ->
        Alcotest.(check bool)
          text
          valid
          (Morbig.Name.is_name name)
    )

let test_cases =
  List.map
    (test_case_is_name ~valid:true)
    [
      "bonjour";
      "BONJOUR";
      "bONjour";
      "bonjour01";
      "bon01jour";
      "_bonjour";
      "_01bonjour";
      "_";
    ]
  @ List.map
    (test_case_is_name ~valid:false)
    [
      "01";
      "bonjour%";
    ]
