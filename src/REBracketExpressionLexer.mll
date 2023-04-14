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

{
  open REBracketExpressionParser
}

rule token = parse
| "-"    { MINUS       }

(*specification:

If an open bracket introduces a bracket expression as in XBD RE
   Bracket Expression, except that the <exclamation-mark> character (
   '!' ) shall replace the <circumflex> character ( '^' ) in its role
   in a non-matching list in the regular expression notation, it shall
   introduce a pattern bracket expression. A bracket expression
   starting with an unquoted <circumflex> character produces
   unspecified results. Otherwise, '[' shall match the character
   itself.

*)
| "!"    { HAT         }
| "["    { LBRACKET    }
| "]"    { RBRACKET    }
| "[="   { Open_equal  }
| "=]"   { Equal_close }
| "[."   { Open_dot    }
| ".]"   { Dot_close   }
| "[:"   { Open_colon  }
| ":]"   { Colon_close }

(** The following rule returns a META_CHAR that will be
    turned into a COLL_ELEM_SINGLE or even a COLL_ELEM_MULTI
    depending on the parsing context. *)
| _ as c { COLL_ELEM_SINGLE c }
| eof    { EOF }
