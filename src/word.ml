(** This module focuses on the language of WORDs.

    This module provides a parsing function that turns a string which
    has been recognized as a word into a word concrete syntax tree.

 *)

open CST

let parse w =
  CST.Word (w, WordOther)
