(**************************************************************************)
(*  -*- tuareg -*-                                                        *)
(*                                                                        *)
(*  Copyright (C) 2017,2018,2019 Yann Régis-Gianas, Nicolas Jeannerod,    *)
(*  Ralf Treinen.                                                         *)
(*                                                                        *)
(*  This is free software: you can redistribute it and/or modify it       *)
(*  under the terms of the GNU General Public License, version 3.         *)
(*                                                                        *)
(*  Additional terms apply, due to the reproduction of portions of        *)
(*  the POSIX standard. Please refer to the file COPYING for details.     *)
(**************************************************************************)

let rec nat_exp k = function
  | -1 -> assert false
  | 0 -> 1
  | 1 -> k
  | n -> let l = nat_exp k (n / 2) in
        l * l * (if n mod 2 = 0 then 1 else k)

let comment f message =
  let y = f () in
  message y;
  y

let string_of_channel cin =
  let b = Buffer.create 16384 in
  let rec aux () =
    Buffer.add_channel b cin 1;
    aux ()
  in
  try
    aux ()
  with
    End_of_file -> Buffer.contents b

let split_list is_delim l =
  let rec aux acc1 acc2 = function
    | [] ->
      List.(rev (rev acc2 :: acc1))
    | x :: xs ->
      if is_delim x then
        aux (List.rev acc2 :: acc1) [] xs
      else
        aux acc1 (x :: acc2) xs
  in
  List.filter (fun x -> x <> []) (aux [] [] l)

let uniq l =
  let rec remove_dup = function
    | [] -> []
    | [x] -> [x]
    | x :: y :: ys when x = y -> remove_dup (y :: ys)
    | x :: ys -> x :: remove_dup ys
  in
  remove_dup (List.sort compare l)

let histogram projector l =
  let similar a b = match a, b with
    | Some (_, a), Some (_, b) -> a = b
    | _, _ -> false
  in
  let rec count c prec = function
    | [] -> []
    | x :: xs ->
      let x' = projector x in
      if similar prec (Some (x, x')) then
        count (c + 1) prec xs
      else match prec with
        | None -> count 1 (Some (x, x')) xs
        | Some (y, _) -> (y, c) :: count 1 (Some (x, x')) xs
  in
  let compare_options a b =
    compare (projector a) (projector b)
  in
  count 0 None (List.sort compare_options l)

let option_iter o f = match o with
  | None -> ()
  | Some x -> f x

let option_map o f = match o with
  | None -> None
  | Some x -> Some (f x)


let string_cut_at k s = String.(
  if length s > k then
    sub s 0 k ^ "..."
  else
    s
)

exception InvalidSuffix of string * string

let string_split k s =
  let n = String.length s in
  let k = min k n in
  try String.sub s 0 k, String.sub s k (n - k) with _ -> assert false

let string_remove_suffix suffix s = String.(
  let k = length s - length suffix in
  if k < 0 then raise (InvalidSuffix (s, suffix));
  let r = try sub s 0 k with _ -> assert false in
  let c = try sub s k (length suffix) with _ -> assert false in
  if suffix <> c then raise (InvalidSuffix (s, suffix));
  r
)

let string_last_char s =
  String.(s.[length s - 1])

let string_minus_last_char s =
  String.(sub s 0 (length s - 1))

(* FIXME: Rename the two following functions. *)
let rec preceded_by n c cs =
  match cs with
  | [] -> n = 0
  | c' :: _cs when n = 0 -> not (c = c')
  | c' :: cs -> c' = c && preceded_by (n - 1) c cs

let rec preceding c cs =
  match cs with
  | [] -> 0
  | c' :: cs -> if c = c' then 1 + preceding c cs else 0

(** [string_to_char_list s] turns a [string s] into a list of [char]. *)
let string_to_char_list s =
  let r = ref [] in
  String.iter (fun c -> r := c :: !r) s;
  List.rev !r

let string_of_char_list s =
  let b = Buffer.create 13 in
  List.iter (Buffer.add_char b) s;
  Buffer.contents b

let count_end_character c s =
  let rec aux r i =
    if i < 0 then r
    else if s.[i] = c then aux (r + 1) (i - 1)
    else r
  in
  aux 0 (String.length s - 1)

(** [strip s] returns a copy of s, without any final newline *)
let string_strip s =
  let n = String.length s in
  if n > 0
  then let lastchar = s.[n-1] in
       if lastchar = '\n'
       then try String.sub s 0 (n-1) with _ -> assert false
       else s
  else s

let reduce f l =
  assert (l <> []);
  let rec aux accu = function
    | [] -> accu
    | x :: xs -> aux (f accu x) xs
  in
  aux (List.hd l) (List.tl l)

let repeat n f =
  let rec aux i =
  if i = n then
    []
  else
    f i :: aux (i + 1)
  in
  aux 0

let rec take n l =
  if n = 0 then [], l else
    match l with
      | [] ->
        [], []
      | x :: xs ->
        let ys, xs = take (n - 1) xs in
        x :: ys, xs

let take_until pred l =
  let rec aux accu = function
  | [] -> [], l
  | x :: xs ->
    if pred x then
      List.rev accu, x :: xs
    else
      aux (x :: accu) xs
  in
  aux [] l

let hashtbl_to_list h =
  let l = ref [] in
  Hashtbl.iter (fun k v -> l := (k, v) :: !l) h;
  !l

let list_hd_opt = function
  | [] -> None
  | x :: _ -> Some x

module FirstSuccessMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val return_if : bool -> 'a -> 'a t
  val fail : 'a t
  val reduce : 'b -> ('b -> 'a -> 'b) -> 'a t list -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( +> ) : 'a t -> 'a t -> 'a t
  val run : 'a t -> 'a option
  val should_succeed :'a t -> 'a
end = struct
  type 'a t = 'a option

  let return x = Some x

  let fail = None

  let return_if b x = if b then return x else fail

  let ( >>= ) x f =
    match x with
      | None -> fail
      | Some x -> f x

  let rec reduce default f = function
    | [] -> return default
    | c :: cs -> c >>= fun a -> reduce (f default a) f cs

  let ( +> ) x y =
    match x with
      | None -> y
      | Some _ -> x

  let run x = x

  exception ShouldHaveSucceeded
  let should_succeed x = match run x with
    | None -> raise ShouldHaveSucceeded
    | Some x -> x
end

(* Pretty-printers helpers *)

let pp_string ppf s =
  Format.fprintf ppf "%s" s

(** [pp_to_print pp] is the pretty-printer [pp] that, instead of
    taking any formatter, uses the std_formatter. *)
let pp_to_print pp =
  pp Format.std_formatter

let pp_to_string pp arg =
  let b = Buffer.create 16 in
  let ppf = Format.formatter_of_buffer b in
  pp ppf arg;
  Format.pp_print_flush ppf ();
  Buffer.contents b

let lexing_make filename contents = Lexing.(
  let lexbuf = Lexing.from_string contents in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  lexbuf
)

let ( <$> ) x f =
  f (); x

let list_last l =
  list_hd_opt (List.rev l)

let newline_regexp =
  Str.regexp "\010"

let lines s =
  Str.split_delim newline_regexp s

let string_last_line s =
  lines s |> list_last
