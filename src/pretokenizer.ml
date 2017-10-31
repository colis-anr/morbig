let make lexbuf =
  let pretokenizer = Prelexer.token [] in

  (**
      The pretokenizer may produce several pretokens, we use an
      intermediate queue to synchronize pretokens' consumption with
      their production.
  *)
  let q = Queue.create () in
  let push x = Queue.push x q in
    let rec aux () =
      try
        Queue.take q
      with Queue.Empty ->
        List.iter (fun x -> Queue.push x q) (pretokenizer lexbuf);
        aux ()
    in
    aux, push
