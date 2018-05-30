module Mat = Owl.Mat
module L = Batteries.List
module LL = Batteries.LazyList
module Seq = Core.Sequence

let always _ = true

type genstate = {time : int ; state : Mat.mat list}

type genstate_seq = genstate Seq.t

(** genstate functions *) 

(** accessor, constructor functions: *)
let time gs = gs.time
let state gs = gs.state
let make time state = {time ; state}

(** genstate_seq functions *) 

let hd = Seq.hd
let tl = Seq.tl
let last = LL.last
let next = Seq.next
let is_empty = Seq.is_empty
let nth = Seq.nth
(* let cons = LL.cons *)
let fold_left = Seq.fold
(* let fold_right = LL.lazy_fold_right *)
let map f xs = Seq.map ~f xs

(* Note as written, f is (x, y) -> z, not x -> y -> z *)
let map2 f xs1 xs2 = Seq.map ~f (Seq.zip xs1 xs2)  (* Is this inefficient?? *)

let from_loop = LL.from_loop
let iterate init f = S.memoize (S.unfold ~init ~f:(fun x -> Some (x, fn x)))
(* or note these examples:
let js = S.unfold_step ~init:0. ~f:(fun x -> S.Step.Yield (x**2., x +. 1.))
let ks = S.unfold_step ~init:0. ~f:(fun x -> if x < 15. then S.Step.Yield (x**2., x +. 1.) else S.Step.Done)
*)
let take = Seq.take
let take_while = Seq.take_while
let drop = Seq.drop
let drop_while = Seq.drop_while
let to_list = Seq.to_list
let rev = LL.rev
(* let nil = LL.nil *)

let lazy_ints ?(skip=1) init_n = iterate init_n (fun n -> n + skip)

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

(*
let lazy_fold_right2 f l1 l2 init_val =
  let rec aux rest1 rest2 =
    lazy begin
      match next rest1, next rest2 with
      | Cons (x1, t1), Cons (x2, t2) -> f x1 x2 (aux t1 t2)
      | Nil, Nil | Nil, _ | _, Nil -> Lazy.force init_val
    end
  in
aux l1 l2
*)

let subseq start finish ll =
  take (finish - start + 1) (drop start ll)

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
