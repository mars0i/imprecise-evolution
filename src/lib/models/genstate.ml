module Mat = Owl.Mat
module L = Batteries.List
module LL = Batteries.LazyList

let always _ = true

type tdists = {gen : int ; dists : Mat.mat list}

type tdistslist = tdists LL.t

(** tdists functions *) 

(** accessor, constructor functions: *)
let gen tds = tds.gen
let dists tds = tds.dists
let make gen dists = {gen ; dists}

(** tdistslist functions *) 

let hd = LL.hd
let tl = LL.tl
let last = LL.last
let next = LL.next
let is_empty = LL.is_empty
let at = LL.at
let cons = LL.cons
let fold_left = LL.fold_left
let fold_right = LL.lazy_fold_right
let map = LL.map
let map2 = LL.map2
let from_loop = LL.from_loop
let iterate init fn = LL.seq init fn always
  (* version for Core.Sequence: let iterate init fn = S.memoize (S.unfold init (fun x -> Some (x, fn x))) *)
let take = LL.take
let take_while = LL.take_while
let drop = LL.drop
let drop_while = LL.drop_while
let to_list = LL.to_list
let rev = LL.rev
let nil = LL.nil

let lazy_range ?(step=1) start stop = 
  let open LL in
  if start = stop then make 1 start
  else let adjust_by, ineq =
    if stop > start then (+), (>) else (-), (<)
  in let rec aux curr stop' =
     if ineq curr stop' then nil
     else lazy (Cons (curr, aux (adjust_by curr step) stop'))
  in aux start stop

let lazy_ints ?(skip=1) init_n =
  iterate init_n (fun n -> n + skip)

let lazy_select accessor keys data =
  let open LL in
  let rec sel ks ds =
    if is_empty ds || is_empty ks then Nil
    else let k, d = hd ks, hd ds in
         let d_key = accessor d in
         if k = d_key then Cons(d, (lzsel (tl ks) (tl ds)))
         else if k > d_key
	 then sel ks (tl ds)  (* let ds catch up *)
         else sel (tl ks) ds  (* let ks catch up *)
  and lzsel ks ds = lazy (sel ks ds)
  in lzsel keys data

(********************************************)
(** Iteration functions *)

let lazy_fold_right2 f l1 l2 init_val =
  let rec aux rest1 rest2 =
    lazy begin
      match next rest1, next rest2 with
      | Cons (x1, t1), Cons (x2, t2) -> f x1 x2 (aux t1 t2)
      | Nil, Nil | Nil, _ | _, Nil -> Lazy.force init_val
    end
  in
aux l1 l2

let take2list n lazy_list = (to_list (take n lazy_list))

let sub_lazy_list start finish ll =
  take (finish - start + 1) (drop start ll)

let take_to_list start finish ll = 
  to_list (sub_lazy_list start finish ll)

let lazy_take_at_idxs ns ll =
  let f acc elt =
    let i, ns', ll' = acc in
    match ns' with
    | [] -> acc
    | n::nstl -> if i = n
                 then (i+1, nstl, cons elt ll')
                 else (i+1, ns', ll')
  in
  let _, _, result = fold_left f (0, ns, nil) ll in
  rev result

let next_intsets pset =
  let n = 1 + L.hd (L.hd pset) in  (* Get next integer; previous one must be first in the first element. *)
  let addl_sets = (L.map (fun xs -> n :: xs) pset) in
  (pset, addl_sets @ pset)

let make_intsets () = from_loop [[0]; []] next_intsets

(** A lazy list of integer power sets. *)
let algebra_sets = make_intsets ()

(*********** probabilities over algebras **********)


let rec subtract_list xs ys =
  match xs, ys with 
  | [], _ -> []
  | _, [] -> xs
  | x::xs', y::ys' when x = y -> subtract_list xs' ys'
  | x::xs', _ -> x::(subtract_list xs' ys)

let list_complement omega_max subset =
  let omega = L.range omega_max `Downto 0 in
  subtract_list omega subset

let prob_sum probs atom_idxs =
  let add_prob sum idx = sum +. Mat.get probs 0 idx  (* Owl.Mat.get rather than .{i,j} to get type right *)
  in L.fold_left add_prob 0. atom_idxs

let invert_prob_sum omega_max atom_extrema subset_idxs = 
  1. -. prob_sum atom_extrema (list_complement omega_max subset_idxs)

(*********** algebras of indexes representing atoms **********)

let algebra_probs probs = 
  let i = (snd (Mat.shape probs)) - 1 in
  let idx_sets = at algebra_sets i in
  let make_entry event = (event, prob_sum probs event) in
  L.map make_entry idx_sets 

let simple_sums omega_max atom_extrema =
  L.map (prob_sum atom_extrema) (at algebra_sets omega_max)

let inverted_sums omega_max atom_extrema =
  L.map (invert_prob_sum omega_max atom_extrema) (at algebra_sets omega_max)

let pri_f_field_lowers omega_max atom_mins atom_maxs =
  let mins = simple_sums omega_max atom_mins in
  let inverted_maxs = inverted_sums omega_max atom_maxs in
  let minmins = L.map2 max mins inverted_maxs in
  L.combine (at algebra_sets omega_max) minmins

let pri_f_field_uppers omega_max atom_mins atom_maxs =
  let maxs = simple_sums omega_max atom_maxs in
  let inverted_mins = inverted_sums omega_max atom_mins in
  let maxmaxs = L.map2 min maxs inverted_mins in
  L.combine (at algebra_sets omega_max) maxmaxs

let ints_from n = iterate n ((+) 1) 

let add_gens ?(first_tick=0) dists_llist =
  map2 make (ints_from first_tick) dists_llist

let remove_gens tdists_llist = map dists tdists_llist

let sublist start_gen finish_gen tdists_llist =
  take_while (fun tds -> tds.gen <= finish_gen)
                (drop_while (fun tds -> tds.gen < start_gen)
		               tdists_llist)

let select_by_gens generations tdists_llist =
  lazy_select gen generations tdists_llist
