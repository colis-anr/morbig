(**specification

   2.6.7 Quote Removal

   The quote characters ( <backslash>, single-quote, and double-quote)
   that were present in the original word shall be removed unless they
   have themselves been quoted.

*)

(** [on_string s] yields a copy of string [s], with all quotes removed
    as described in the specification. *)
let on_string s =
  let n = String.length s in
  let b = Buffer.create n in
  let i = ref 0 in
  let keep () = Buffer.add_char b s.[!i]; incr i
  and skip () = incr i in
  while !i<n do
    if s.[!i] = '\''
    then begin
        (* skip the initial single quote *)
        skip ();
        (* scan and push on the buffer until next single quote *)
        while (!i<n && s.[!i] <> '\'') do
          keep ()
        done;
        (* skip the final single quote *)
        if !i<n then skip ()
      end
    else if s.[!i] = '"'
    then
      (* just skip any double quote if we see it here (that is, not escaped
           and not inside single quotes *)
      skip ()
    else if s.[!i] = '\\'
    then begin
        (* skip the backslash *)
        skip ();
        (* and push the next symbol on the buffer *)
        if !i<n then keep ()
      end
    else keep ()
  done;
  Buffer.contents b
