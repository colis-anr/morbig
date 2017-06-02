(* FIXME: Naive implementation. *)

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
  with _ -> Buffer.contents b

let lines_of_channel cin =
  let lines = ref [] in
  let lineno = ref 0 in
  (try while true do
      incr lineno;
      lines := (!lineno, input_line cin) :: !lines
   done with _ -> ());
  List.rev !lines

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

let string_cut_at k s = String.(
  if length s > k then
    sub s 0 k ^ "..."
  else
    s
)

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

let rec take_until pred = function
  | [] -> [], []
  | x :: xs ->
    if pred x then
      let ys, xs = take_until pred xs in
      x :: ys, xs
    else
      [], x :: xs

let hashtbl_to_list h =
  let l = ref [] in
  Hashtbl.iter (fun k v -> l := (k, v) :: !l) h;
  !l

module FirstSuccessMonad : sig
  type 'a t
  val return : 'a -> 'a t
  val fail : 'a t
  val reduce : 'b -> ('b -> 'a -> 'b) -> 'a t list -> 'b t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( +> ) : 'a t -> 'a t -> 'a t
  val run : 'a t -> 'a option
  val should_succeed :'a t -> 'a
end = struct
  type 'a t = 'a option Lazy.t

  let return x = lazy (Some x)

  let fail = lazy None

  let ( >>= ) x f =
    match Lazy.force x with
      | None -> fail
      | Some x -> f x

  let rec reduce default f = function
    | [] -> return default
    | c :: cs -> c >>= fun a -> reduce (f default a) f cs

  let ( +> ) x y =
    match Lazy.force x with
      | None -> y
      | Some _ -> x

  let run x = Lazy.force x

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
