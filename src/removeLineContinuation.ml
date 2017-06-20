(**

   The following module implements the removal of line continuation.

*)

(**specification

   2.2.1 Escape Character (Backslash)

   A <backslash> that is not quoted shall preserve the literal value of
   the following character, with the exception of a <newline>. If a
   <newline> follows the <backslash>, the shell shall interpret this as
   line continuation. The <backslash> and <newline> shall be removed
   before splitting the input into tokens. Since the escaped <newline> is
   removed entirely from the input and is not replaced by any white
   space, it cannot serve as a token separator.

*)

let backslash_newline =
  Str.regexp "\\\\\\(\010\\|\013\\|\013\010\\)"

let transform s =
  Str.global_replace backslash_newline "" s
