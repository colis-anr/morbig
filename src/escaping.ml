open ExtPervasives
open Nesting

(**
   A double quote can be escaped if we are already inside (at least)
   two levels of quotation. For instance, if the input is <dquote>
   <dquote> <backslash><backslash> <dquote> <dquote> <dquote>, the
   escaped backslash is used to escape the quote character.

*)
let escape_analysis level current =
  let current =
    List.map
      (function
       | PrelexerState.WordComponent (s, _) -> s
       | _ -> "")
      current.PrelexerState.buffer
  in
  let number_of_backslashes_to_escape = Nesting.(
    (* FIXME: We will be looking for the general pattern here. *)
    match level with
    | DQuotes :: Backquotes ('`', _) :: DQuotes :: _ -> 2
    | DQuotes :: Backquotes ('`', _) :: _ :: DQuotes :: _ -> 2
    | DQuotes :: Backquotes ('`', _) :: _ -> 1
    | Backquotes ('`', _) :: DQuotes :: _ -> 2
    | Backquotes ('`', _) :: _ :: DQuotes :: _ -> 2
    | _ -> 1
  )
  in
  let escape_sequence =
    repeat number_of_backslashes_to_escape (fun _ -> '\\')
  in

  let remove_escaped_backslashes current =
    let rec trim = function
      | [] ->
         []
      | '\\' :: cs ->
         let cs' = trim cs in
         if fst (take number_of_backslashes_to_escape cs')
            = escape_sequence
         then
           snd (take number_of_backslashes_to_escape cs')
         else
           '\\' :: cs'
      | c :: cs ->
         c :: trim cs
    in
    trim current
  in
  let current' = List.(concat (map rev (map string_to_char_list current))) in
  let current' =
    (* FIXME: Justify this! *)
    if not (under_backquoted_style_command_substitution level) then
      remove_escaped_backslashes current'
    else
      current'
  in
  if preceded_by number_of_backslashes_to_escape '\\' current' then
    (** There is no special meaning for this character. It is
        escaped. *)
    None
  else
    (**
        The character preceded by this sequence is not escaped.
        In the case of `, the interpretation of this character
        depends on the number of backslashes the precedes it.
        Typically, in:

        echo `echo \`foo\``

        The second <backquote> is not escaped BUT it is not
        closing the current subshell, it is opening a new
        one.

     *)
    Some number_of_backslashes_to_escape

let escape_analysis_predicate level current =
  escape_analysis level current = None

let escaped_double_quote = escape_analysis_predicate

let escaped_single_quote = escape_analysis_predicate

let escaped_backquote = escape_analysis

