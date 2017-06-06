(** Extension of standard library's positions. *)

(** {2 Extended lexing positions} *)

(** Abstract type for pairs of positions in the lexing stream. *)
type t
[@@deriving yojson]

type position = t
[@@deriving yojson]

(** Decoration of a value with a position. *)
type 'a located =
    {
      value    : 'a;
      position : t;
    }
[@@deriving yojson]

(** [value dv] returns the raw value that underlies the
    decorated value [dv]. *)
val value: 'a located -> 'a

(** [position dv] returns the position that decorates the
    decorated value [dv]. *)
val position: 'a located -> t

(** [destruct dv] returns the couple of position and value
    of a decorated value [dv]. *)
val destruct: 'a located -> 'a * t

(** [located f x] applies [f] to the value of [x]. *)
val located : ('a -> 'b) -> 'a located -> 'b

(** [with_pos p v] decorates [v] with a position [p]. *)
val with_pos : t -> 'a -> 'a located

(** [with_cpos p v] decorates [v] with a lexical position [p]. *)
val with_cpos: Lexing.lexbuf -> 'a -> 'a located

(** [with_poss start stop v] decorates [v] with a position [(start, stop)]. *)
val with_poss : Lexing.position -> Lexing.position -> 'a -> 'a located

(** [unknown_pos x] decorates [v] with an unknown position. *)
val unknown_pos : 'a -> 'a located

(** This value is used when an object does not come from a particular
    input location. *)
val dummy: t

(** [map f v] extends the decoration from [v] to [f v]. *)
val map: ('a -> 'b) -> 'a located -> 'b located

(** [iter f dv] applies [f] to the value inside [dv]. *)
val iter: ('a -> unit) -> 'a located -> unit

(** [mapd f v] extends the decoration from [v] to both members of the pair
    [f v]. *)
val mapd: ('a -> 'b1 * 'b2) -> 'a located -> 'b1 located * 'b2 located

(** {2 Accessors} *)

(** [start_of_position p] returns the beginning of a position [p]. *)
val start_of_position: t -> Lexing.position

(** [end_of_position p] returns the end of a position [p]. *)
val end_of_position: t -> Lexing.position

(** [filename_of_position p] returns the filename of a position [p]. *)
val filename_of_position: t -> string

(** {2 Position handling} *)

(** [join p1 p2] returns a position that starts where [p1]
    starts and stops where [p2] stops. *)
val join : t -> t -> t

(** [lex_join l1 l2] returns a position that starts at [l1] and stops
    at [l2]. *)
val lex_join : Lexing.position -> Lexing.position -> t

(** [string_of_lex_pos p] returns a string representation for
    the lexing position [p]. *)
val string_of_lex_pos : Lexing.position -> string

(** [string_of_pos p] returns the standard (Emacs-like) representation
    of the position [p]. *)
val string_of_pos : t -> string

(** [pos_or_undef po] is the identity function except if po = None,
    in that case, it returns [undefined_position]. *)
val pos_or_undef : t option -> t

(** {2 Interaction with the lexer runtime} *)

(** [cpos lexbuf] returns the current position of the lexer. *)
val cpos : Lexing.lexbuf -> t

(** [string_of_cpos p] returns a string representation of
    the lexer's current position. *)
val string_of_cpos : Lexing.lexbuf -> string
