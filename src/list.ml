
open Printf

(* almost like stdlib's List module except that the empty list type does not
   exist; so you never get an empty list exception when using
   this module; neither can you construct an empty list *)

type 'a t = One of 'a
          | More of 'a * 'a t

let rec length_aux acc = function
  | One _ -> acc + 1
  | More (_, xs) -> length_aux (acc + 1) xs

(* val length 'a list -> int *)
let length l =
  length_aux 0 l

(* val compare_lengths 'a list -> 'b list -> int *)
let compare_lengths l1 l2 =
  compare (length l1) (length l2)

(* val compare_length_with 'a list -> int -> int *)
(* UNIMP *)

(* val cons 'a -> 'a list -> 'a list *)
let cons x l =
  More (x, l)

(* val hd 'a list -> 'a *)
let hd = function
  | One x -> x
  | More (x, _) -> x

(* val tl 'a list -> 'a list *)
(* FORBIDDEN (could create an empty list) *)

(* val nth 'a list -> int -> 'a *)
let nth l i =
  if i < 0 then
    raise (Invalid_argument (sprintf "Nelist.nth l %d: %d < 0" i i));
  let rec loop j = function
    | One x ->
      if j = 0 then x
      else raise (Failure (sprintf "Nelist.nth l %d: l is too short" i))
    | More (x, xs) ->
      if j = 0 then x
      else loop (j - 1) xs
  in
  loop i l

(* val nth_opt 'a list -> int -> 'a option *)
let nth_opt l i =
  try Some (nth l i)
  with _ -> None

let of_std_list l =
  match List.rev l with
  | [] -> assert(false) (* forbidden *)
  | x :: xs ->
    List.fold_left (fun acc x -> cons x acc) (One x) xs

let to_std_list l =
  let rec loop acc = function
    | One x -> List.rev (x :: acc)
    | More (x, xs) -> loop (x :: acc) xs
  in
  loop [] l

(* val rev 'a list -> 'a list *)
let rev l =
  let rec loop acc = function
    | One x -> More (x, acc)
    | More (x, xs) -> loop (More (x, acc)) xs
  in
  match l with
  | One x -> One x
  | More (x, xs) -> loop (One x) xs

(* val init int -> (int -> 'a) -> 'a list *)
let init len f =
  assert(len > 0);
  let rec loop i acc =
    if i = 0 then More (f 0, acc)
    else loop (i - 1) (More (f i, acc))
  in
  if len = 1 then One (f 0)
  else (* len > 1 *)
    loop (len - 2) (One (f (len - 1)))

(* val rev_append 'a list -> 'a list -> 'a list *)
let rec rev_append l1 acc =
  match l1 with
  | One x -> More (x, acc)
  | More (x, xs) -> rev_append xs (More (x, acc))

(* val append 'a list -> 'a list -> 'a list *)
let append l1 l2 =
  let l1' = rev l1 in
  rev_append l1' l2

(* val fold_left ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f init l =
  let rec loop acc = function
    | One x -> f acc x
    | More (x, xs) -> loop (f acc x) xs
  in
  loop init l

(** alias for fold_left *)
let fold = fold_left

(* val concat 'a list list -> 'a list *)
let concat l =
  let l' = rev l in
  match l' with
  | One x -> x
  | More (x, xs) -> failwith "not implemented yet"

(* val flatten 'a list list -> 'a list *)
let flatten = concat

(* val iter ('a -> unit) -> 'a list -> unit *)
let iter f l =
  let rec loop = function
    | One x -> f x
    | More (x, xs) -> (f x; loop xs)
  in
  loop l

(* val iteri (int -> 'a -> unit) -> 'a list -> unit *)
let iteri f l =
  let rec loop i = function
    | One x -> f i x
    | More (x, xs) -> (f i x; loop (i + 1) xs)
  in
  loop 0 l

(* val rev_map ('a -> 'b) -> 'a list -> 'b list *)
let rev_map f l =
  let rec loop acc = function
    | One x -> cons (f x) acc
    | More (x, xs) -> loop (cons (f x) acc) xs
  in
  match l with
  | One x -> One (f x)
  | More (x, xs) -> loop (One (f x)) xs

(* val map ('a -> 'b) -> 'a list -> 'b list *)
let map f l =
  rev_map f (rev l)

(* val mapi (int -> 'a -> 'b) -> 'a list -> 'b list *)
let mapi f l =
  let i = ref 0 in
  let f' =
    let res = f !i in
    incr i;
    res
  in
  rev_map f' (rev l)

(* val fold_right ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let fold_right f l =
  let g acc x =
    f x acc
  in
  fold_left g (rev l)

(* val iter2 ('a -> 'b -> unit) -> 'a list -> 'b list -> unit *)
let iter2 f li1 li2 =
  let rec loop l1 l2 = match l1, l2 with
    | One x, One y -> f x y
    | More (x, xs), More (y, ys) -> (f x y; loop xs ys)
    | _ -> raise (Invalid_argument "Nelist.iter2: different list lengths")
  in
  loop li1 li2

(* val rev_map2 ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
let rev_map2 f li1 li2 =
  let rec loop acc l1 l2 = match l1, l2 with
    | One x, One y -> More (f x y, acc)
    | More (x, xs), More (y, ys) -> loop (More (f x y, acc)) xs ys
    | _ -> raise (Invalid_argument "Nelist.rev_map2: different list lengths")
  in
  match li1, li2 with
  | One x, One y -> One (f x y)
  | More (x, xs), More (y, ys) -> loop (One (f x y)) xs ys
  | _ -> raise (Invalid_argument "Nelist.rev_map2: different list lengths")

(* val map2 ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
let map2 f l1 l2 =
  rev (rev_map2 f l1 l2)

(* val fold_left2 ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a *)
(* TODO *)

(* val fold_right2 ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c *)
(* TODO *)

(* val for_all ('a -> bool) -> 'a list -> bool *)
let for_all p l =
  let rec loop = function
    | One x -> p x
    | More (x, xs) -> p x && loop xs
  in
  loop l

(* val exists ('a -> bool) -> 'a list -> bool *)
let exists p l =
  let rec loop = function
    | One x -> p x
    | More (x, xs) -> p x || loop xs
  in
  loop l

(* val for_all2 ('a -> 'b -> bool) -> 'a list -> 'b list -> bool *)
let for_all2 p li1 li2 =
  let rec loop = function
    | (One x, One y) -> p x y
    | (More (x, xs), More (y, ys)) -> (p x y) && loop (xs, ys)
    | _ -> raise (Invalid_argument "Nelist.for_all2: different list lengths")
  in
  loop (li1, li2)

(* val exists2 ('a -> 'b -> bool) -> 'a list -> 'b list -> bool *)
let exists2 p li1 li2 =
  let rec loop = function
    | (One x, One y) -> p x y
    | (More (x, xs), More (y, ys)) -> (p x y) || (loop (xs, ys))
    | _ -> raise (Invalid_argument "Nelist.exists2: different list lengths")
  in
  loop (li1, li2)

(* val mem 'a -> 'a list -> bool *)
let mem x l =
  exists ((=) x) l

(* val memq 'a -> 'a list -> bool *)
let memq x l =
  exists ((==) x) l

(* val find ('a -> bool) -> 'a list -> 'a *)
let find p l =
  let rec loop = function
    | One x -> if p x then x else raise Not_found
    | More (x, xs) -> if p x then x else loop xs
  in
  loop l

(* val find_opt ('a -> bool) -> 'a list -> 'a option *)
let find_opt p l =
  try Some (find p l)
  with Not_found -> None

(* val split ('a * 'b) list -> 'a list * 'b list *)
let split l =
  let rec loop acc1 acc2 = function
    | One (x, y) -> (rev (More (x, acc1)), rev (More (y, acc2)))
    | More ((x, y), xys) -> loop (More (x, acc1)) (More (y, acc2)) xys
  in
  match l with
  | One (x, y) -> (One x, One y)
  | More ((x, y), xys) -> loop (One x) (One y) xys

(* val combine 'a list -> 'b list -> ('a * 'b) list *)
let combine li1 li2 =
  let rec loop acc xs ys = match xs, ys with
    | (One x, One y) -> rev (More ((x, y), acc))
    | (More (x, xs), More (y, ys)) -> loop (More ((x, y), acc)) xs ys
    | _ -> raise (Invalid_argument "Nelist.combine: different list lengths")
  in
  match li1, li2 with
  | (One x, One y) -> One (x, y)
  | (More (x, xs), More (y, ys)) -> loop (One (x, y)) xs ys
  | _ -> raise (Invalid_argument "Nelist.combine: different list lengths")

let of_array a =
  failwith "not implemented yet"

(* val sort ('a -> 'a -> int) -> 'a list -> 'a list *)
let sort cmp = function
  | One x -> One x
  | (More (x, xs) as l) ->
    let n = length l in
    let a = Array.make n x in
    iteri (fun i y ->
        Array.unsafe_set a i y
      ) l;
    Array.sort cmp a;
    of_array a

(* val stable_sort ('a -> 'a -> int) -> 'a list -> 'a list *)
(* TODO *)

(* val fast_sort ('a -> 'a -> int) -> 'a list -> 'a list *)
(* TODO *)

(* val sort_uniq ('a -> 'a -> int) -> 'a list -> 'a list *)
(* TODO *)

(* val merge ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list *)
(* TODO *)

(* val filter ('a -> bool) -> 'a list -> 'a list *)
(* FORBIDDEN *)

(* val find_all ('a -> bool) -> 'a list -> 'a list *)
(* FORBIDDEN *)

(* val partition ('a -> bool) -> 'a list -> 'a list * 'a list *)
(* FORBIDDEN *)

(* val assoc 'a -> ('a * 'b) list -> 'b *)
(* USELESS *)

(* val assoc_opt 'a -> ('a * 'b) list -> 'b option *)
(* USELESS *)

(* val assq 'a -> ('a * 'b) list -> 'b *)
(* USELESS *)

(* val assq_opt 'a -> ('a * 'b) list -> 'b option *)
(* USELESS *)

(* val mem_assoc 'a -> ('a * 'b) list -> bool *)
(* USELESS *)

(* val mem_assq 'a -> ('a * 'b) list -> bool *)
(* USELESS *)

(* val remove_assoc 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* USELESS *)

(* val remove_assq 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* USELESS *)
