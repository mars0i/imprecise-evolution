module Mat = Owl.Mat
module L = Batteries.List
module LL = Batteries.LazyList
module Seq = Core.Sequence

let (%) f g = (fun x -> f (g x))

let always _ = true

type genstate = {time : int ; state : Mat.mat list}

type genstate_seq = genstate Seq.t

(** genstate functions *) 

(** accessor, constructor functions: *)
let time gs = gs.time
let state gs = gs.state
let make time state = {time ; state}

(** genstate_seq functions *) 

(* NOTE these examples:
numbers increasing as powers of 2:
let ys = Seq.unfold_step ~init:1 ~f:(fun s -> Ss.Yield (s, s * 2))
infinite sequence of squares:
let js = Seq.unfold_step ~init:0. ~f:(fun x -> Seq.Step.Yield (x**2., x +. 1.))
Stop when reach 15:
let ks = Seq.unfold_step ~init:0. ~f:(fun x -> if x < 15. then Seq.Step.Yield (x**2., x +. 1.) else Seq.Step.Done)
after 3, the next number is 14:
let zs = Seq.unfold_step ~init:1 ~f:(fun x -> if x = 4 then Ss.Skip 14 else Ss.Yield (x, x + 1));;
*)

let hd = Seq.hd_exn
let tl = Seq.tl_eagerly_exn
let cons x xs = Seq.(append (singleton x) xs)
let next = Seq.next
let is_empty = Seq.is_empty
let nth = Seq.nth
let fold_left = Seq.fold
let map f xs = Seq.map ~f xs

(* let map2 f xs ys = Seq.map ~f:(fun (x, y) -> f x y) (Seq.zip xs ys) *)

(* Based on Yaron Minsky's def at https://discuss.ocaml.org/t/how-to-write-map2-for-base-sequence/2090/3?u=mars0i *)
let map2 f s1 s2 =
  Seq.unfold ~init:(s1,s2)
    ~f:(fun (s1,s2) ->
      match Seq.next s1, Seq.next s2 with
      | None, _ | _, None -> None
      | Some (x1, rest1), Some (x2, rest2) -> Some (f x1 x2, (rest1, rest2)))
(* Notes: *)

let iterate init f = Seq.memoize (Seq.unfold ~init ~f:(fun x -> Some (x, f x)))
let take = Seq.take
let take_while = Seq.take_while
let drop = Seq.drop
let drop_while = Seq.drop_while
let to_list = Seq.to_list

(* Note Core.Sequence.range doesn't allow an unbounded sequence. *)
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

let subseq start finish ll = Seq.sub ll start (finish - start + 1)

let ints_from n = iterate n ((+) 1) 

let add_times ?(first_tick=0) genstate_seq =
  map2 make (ints_from first_tick) genstate_seq

let remove_times genstate_seq = map state genstate_seq

let sublist start_time finish_time genstate_seq =
  Seq.take_while ~f:(fun gs -> gs.time <= finish_time)
                 (Seq.drop_while ~f:(fun gs -> gs.time < start_time)
		                 genstate_seq)

let select_by_times generations genstate_seq =
  lazy_select time generations genstate_seq
