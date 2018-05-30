module Mat = Owl.Mat
module L = Batteries.List
module LL = Batteries.LazyList

let always _ = true

type genstate = {time : int ; state : Mat.mat list}

type genstate_seq = genstate LL.t

(** genstate functions *) 

(** accessor, constructor functions: *)
let time gs = gs.time
let state gs = gs.state
let make time state = {time ; state}

(** genstate_seq functions *) 

let hd = LL.hd
let tl = LL.tl
let last = LL.last
let next = LL.next
let is_empty = LL.is_empty
let nth = LL.nth
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

let sub_lazy_list start finish ll =
  take (finish - start + 1) (drop start ll)

(*
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
*)

let next_intsets pset =
  let n = 1 + L.hd (L.hd pset) in  (* Get next integer; previous one must be first in the first element. *)
  let addl_sets = (L.map (fun xs -> n :: xs) pset) in
  (pset, addl_sets @ pset)

let make_intsets () = from_loop [[0]; []] next_intsets

(** A lazy list of integer power sets. *)
let algebra_sets = make_intsets ()

(*********** probabilities over algebras **********)

let ints_from n = iterate n ((+) 1) 

let add_times ?(first_tick=0) genstate_seq =
  map2 make (ints_from first_tick) genstate_seq

let remove_times genstate_seq = map state genstate_seq
let sublist start_time finish_time genstate_seq =
  take_while (fun gs -> gs.time <= finish_time)
                (drop_while (fun gs -> gs.time < start_time)
		               genstate_seq)

let select_by_times generations genstate_seq =
  lazy_select time generations genstate_seq
