let message what consequence ?filename msg =
  Printf.eprintf "%s[%s] %s\n"
    (match filename with None -> "" | Some f -> f ^":0:0:\n")
    what
    msg;
  consequence ()

let warning = message "WARNING" ignore
let error = message "ERROR" (fun () -> exit 1)
