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
  let ignore_commandf format =
    Format.ksprintf (fun string -> ignore (Sys.command string)) format
end

let bat_or_cat path =
  Sys.ignore_commandf
    "command -v bat >/dev/null && bat --force-colorization --style numbers %s || cat %s"
    (Filename.quote path) (Filename.quote path)

let skip_if_no_input path =
  if not (Sys.file_exists (Filename.concat path "input.sh")) then
    (
      (* FIXME: these tests should not be skipped but fixed or deleted. *)
      pf "Test does not have an `input.sh`. Skipping.@.";
      Alcotest.skip ()
    )

let print_input path =
  pf "Input is:@\n@.";
  bat_or_cat (Filename.concat path "input.sh");
  pf "@."

let skip_if_open path =
  if Sys.file_exists (Filename.concat path "open") then
    (
      pf "Test has an open issue corresponding to it. Skipping.@.";
      Alcotest.skip ()
    )

let run_morbig path =
  let qpath = Filename.quote path in
  pf "Running Morbig...@.";
  let rc = Sys.commandf "%s --as simple %s/input.sh" !morbig_path qpath in
  pf "Morbig terminated with return code %d.@." rc;
  (* normalise with `jq` into `output.json` *)
  Sys.ignore_commandf "if [ -e %s/input.sh.sjson ]; then cat %s/input.sh.sjson | jq . > %s/output.json; fi" qpath qpath qpath;
  rc

let print_output path =
  pf "Output is:@\n@.";
  bat_or_cat (Filename.concat path "output.json");
  pf "@."

let print_expected path =
  pf "Expected is:@\n@.";
  bat_or_cat (Filename.concat path "expected.json");
  pf "@."

let compare_outputs path =
  let qpath = Filename.quote path in
  Sys.commandf "diff %s/expected.json %s/output.json >/dev/null" qpath qpath

let print_diff path =
  let qpath = Filename.quote path in
  pf "Diff is (\027[31m< expected\027[0m | \027[32m> output\027[0m):@\n@.";
  Sys.ignore_commandf "diff --color=always %s/expected.json %s/output.json" qpath qpath;
  pf "@."

let check_bad_test_case path = fun () ->
  skip_if_no_input path;
  print_input path;
  skip_if_open path;
  if run_morbig path = 0 then
    (
      print_output path;
      Alcotest.fail "Morbig was supposed to fail and succeeded instead"
    )

let check_good_test_case path = fun () ->
  skip_if_no_input path;
  print_input path;
  skip_if_open path;
  if run_morbig path != 0 then
    Alcotest.fail "Morbig was supposed to succeed and failed instead";
  print_output path;
  print_expected path;
  if compare_outputs path != 0 then
    (
      print_diff path;
      Alcotest.fail "Morbig's output isn't as expected"
    )

let rec collect_test_paths dir =
  List.flatten
    (
      List.map
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
    )

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
