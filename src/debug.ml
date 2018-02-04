let debug =
  ref false

let printf fmt =
  if !debug then Printf.eprintf fmt else ignore
