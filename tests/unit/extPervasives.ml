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
        ~name:"string_minus_last_char ^ string_last_char = id"
        string
        (fun s ->
           assume (s <> "");
           s = MEP.(string_minus_last_char s
                    ^ String.make 1 (string_last_char s))
        )
    )

module List = struct
  module MEPL = Morbig__.ExtPervasives.List

  let bd_tl =
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make
          ~count:1000
          ~name:"List.(bd @ [tl] = id)"
          (list int)
          (fun l ->
             assume (List.compare_length_with l 2 >= 0);
             l = MEPL.(bd l @ [ft l])
          )
      )

  let hd_cr_tl =
    QCheck_alcotest.to_alcotest
      QCheck.(
        Test.make
          ~count:1000
          ~name:"List.([hd] @ cr @ [tl] = id)"
          (list int)
          (fun l ->
             assume (List.compare_length_with l 2 >= 0);
             l = MEPL.([hd l] @ cr l @ [ft l])
          )
      )

  let test_cases = [
    bd_tl;
    hd_cr_tl;
  ]
end

let test_cases = [
  smlc_slc;
] @ List.test_cases
