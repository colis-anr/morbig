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

module MEP = Morbig__.ExtPervasives

let smlc_slc =
  QCheck_alcotest.to_alcotest
    QCheck.(
      Test.make
        ~count:1000
        ~name:"string_minus_last_char ^ string_last_char"
        string
        (fun s ->
           assume (s <> "");
           s = MEP.(string_minus_last_char s
                    ^ String.make 1 (string_last_char s))
        )
    )

let test_cases = [
  smlc_slc;
]
