open ExtPervasives
open CST

module Lexer (U : sig end) = struct

  let on_next_line   = ref false
  let lexing         = ref false
  let delimiters     = ref ([] : string list)
  let skip_tabs      = ref ([] : bool list)
  let find_delimiter = ref false
  let placeholders   = ref ([] : word located ref list)

  let fill_next_here_document_placeholder here_document =
    assert (!placeholders <> []);
    (List.hd !placeholders) := here_document;
    placeholders := List.tl !placeholders

  let next_here_document lexbuf =
    assert (!delimiters <> []);
    assert (!skip_tabs <> []);
    let delimiter = List.hd !delimiters
    and skip_tabs_flag = List.hd !skip_tabs
    and doc = Buffer.create 1000
    and nextline, pstart, pstop =
      match Prelexer.readline lexbuf with
        | None -> failwith "Unterminated here document."
        | Some (l, b, e) -> (ref l, ref b, ref e)
    in
    while (string_strip (
               if skip_tabs_flag then string_untab !nextline else !nextline
             ) <> delimiter)
    do
      Buffer.add_string doc !nextline;
      match Prelexer.readline lexbuf with
        | None -> failwith "Unterminated here document."
        | Some (l,b,e) -> nextline := l;
          pstop := e
    done;
    delimiters := List.tl !delimiters;
    skip_tabs := List.tl !skip_tabs;
    if !delimiters = [] then lexing := false;
    let before_stop = Lexing.({ !pstop with
      pos_cnum = !pstop.pos_cnum - 1;
      pos_bol  = !pstop.pos_bol  - 1;
    }) in
    fill_next_here_document_placeholder (CST.{
        value = Word (Buffer.contents doc);
        position = { start_p = CSTHelpers.internalize !pstart;
                     end_p = CSTHelpers.internalize !pstop }
    });
    (Prelexer.NEWLINE, before_stop, !pstop)

  let initiate_here_document_lexing_on_next_line dashed r =
    on_next_line := true;
    find_delimiter := true;
    placeholders := r :: !placeholders;
    skip_tabs := dashed :: !skip_tabs


end
