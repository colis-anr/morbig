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

(** Generates a string and an index comprised within the string. *)
let gen_string_and_index =
  QCheck2.Gen.(
    string >>= fun s ->
    int_range 0 (String.length s) >>= fun n ->
    return (n, s)
  )

let string_split_id =
  QCheck_alcotest.to_alcotest
    QCheck2.(
      Test.make
        ~count:1000
        ~name:"(^) % string_split = id"
        gen_string_and_index
        (fun (k, s) ->
           let (s1, s2) = MEP.string_split k s in
           s = s1 ^ s2)
    )

let string_split_right_size =
  QCheck_alcotest.to_alcotest
    QCheck2.(
      Test.make
        ~count:1000
        ~name:"string_split has right size"
        gen_string_and_index
        (fun (k, s) ->
           let (s1, _) = MEP.string_split k s in
           String.length s1 = k)
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
  string_split_id;
  string_split_right_size;
] @ List.test_cases
