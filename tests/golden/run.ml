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

(* FIXME: The following is a quick hack. The right way to go would be to use
   Alcotest's [run_with_args] function and its [Cmdliner] argument. *)

let morbig_path = ref "you-need-to-set-the-morbig-command-line-argument"
let () =
  Arg.parse
    ["--morbig", (Arg.Set_string morbig_path), ""]
    (fun _anon -> assert false)
    ""

let pf = Format.printf
let spf = Format.sprintf

module Sys = struct
  include Sys
  let commandf format = Format.ksprintf Sys.command format
end

let check_bad_test_case path = fun () ->
  let qpath = Filename.quote path in
  if Sys.file_exists (Filename.concat path "open") then
    (
      pf "Test has an open issue corresponding to it. Skipping.@.";
      Alcotest.skip ()
    );
  (
    pf "Running Morbig...@.";
    let rc = Sys.commandf "%s %s/input.sh" !morbig_path qpath in
    pf "Morbig terminated with return code %d.@." rc;
    if rc = 0 then Alcotest.fail "Morbig was supposed to fail and succeeded instead"
  )

let check_good_test_case path = fun () ->
  let qpath = Filename.quote path in
  if Sys.file_exists (Filename.concat path "open") then
    (
      pf "Test has an open issue corresponding to it. Skipping.@.";
      Alcotest.skip ()
    );
  if not (Sys.file_exists (Filename.concat path "input.sh")) then
    (
      (* FIXME: these tests should not be skipped but fixed or deleted. *)
      pf "Test does not have an `input.sh`. Skipping.@.";
      Alcotest.skip ()
    );
  (
    pf "Running Morbig...@.";
    let rc = Sys.commandf "%s %s/input.sh" !morbig_path qpath in
    pf "Morbig terminated with return code %d.@." rc;
    if rc != 0 then Alcotest.fail "Morbig was supposed to succeed and failed instead"
  );
  (
    ignore (Sys.commandf "cat %s/input.sh.sjson | jq . > %s/input.sh.sjson.clean" qpath qpath);
    ignore (Sys.commandf "mv %s/input.sh.sjson.clean %s/input.sh.sjson" qpath qpath);
    let rc = Sys.commandf "diff %s/input.sh.sjson %s/expected.json 2>&1 >/dev/null" qpath qpath in
    if rc != 0 then Alcotest.fail "Diff is not happy with Morbig's output"
  )

let rec collect_test_paths dir =
  List.concat_map
    (
      fun file ->
        let file = Filename.concat dir file in
        if Filename.check_suffix file ".t" then
          [file]
        else if Sys.is_directory file then
          collect_test_paths file
        else
          []
    )
    (Array.to_list (Sys.readdir dir))

let bad_test_cases =
  List.map
    (
      fun path ->
        Alcotest.(test_case path `Quick (check_bad_test_case path))
    )
    (collect_test_paths "bad")

let good_test_cases =
  List.map
    (fun path ->
       Alcotest.(test_case path `Quick (check_good_test_case path))
    )
    (collect_test_paths "good")

let () =
  Alcotest.run
    ~argv:[|"unused"|] (* FIXME: quick hack; cf top of this file *)
    "golden"
    [
      ("bad", bad_test_cases);
      ("good", good_test_cases);
    ]
