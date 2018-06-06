module Mat = Owl.Mat
module L = Batteries.List
module LL = Batteries.LazyList
module S = Core.Sequence
module Ss = Core.Sequence.Step

let (%) f g = (fun x -> f (g x))

let always _ = true

type genstate = {time : int ; state : Mat.mat list}

type genstate_seq = genstate S.t

(** genstate functions *) 

(** accessor, constructor functions: *)
let time gs = gs.time
let state gs = gs.state
let make time state = {time ; state}

(** genstate_seq functions *) 

let hd = S.hd_exn
let tl = S.tl_eagerly_exn
let cons x xs = S.(append (singleton x) xs)
let next = S.next
let is_empty = S.is_empty
let nth = S.nth_exn
let fold_left f init seq = S.fold ~f ~init seq
let map f xs = S.map ~f xs
let iterate init f = S.memoize (S.unfold ~init ~f:(fun x -> Some (x, f x)))
let take n seq = S.take seq n
let take_while f seq = S.take_while ~f seq
let drop n seq = S.drop seq n
let drop_while f seq = S.drop_while ~f seq
let to_list = S.to_list

(* Note Core.Sequence.range doesn't allow an unbounded sequence. *)
let ints ?(stride=1) init_n = iterate init_n (fun n -> n + stride)

(* let map2 f xs ys = S.map ~f:(fun (x, y) -> f x y) (S.zip xs ys) *)

(* Based on Yaron Minsky's def at https://discuss.ocaml.org/t/how-to-write-map2-for-base-sequence/2090/3?u=mars0i *)
let map2 f s1 s2 =
  S.unfold ~init:(s1,s2)
    ~f:(fun (s1,s2) ->
      match S.next s1, S.next s2 with
      | None, _ | _, None -> None
      | Some (x1, rest1), Some (x2, rest2) -> Some (f x1 x2, (rest1, rest2)))

(**
Example:
{[
    let vals = S.zip (S.repeat "foo") (S.range 0 10)
    let keys = S.of_list [2;3;5]
    select snd keys vals2 |> S.to_list
    - : (string * int) list = [("foo", 2); ("foo", 3); ("foo", 5)]
]}
*)

(* There's probably a more idiomatic way to do this. *)
let select accessor keys vals =
  S.unfold_step ~init:(keys, vals)
                ~f:(fun (ks, vs) -> 
		    if S.is_empty ks || S.is_empty vs then Ss.Done 
		    else if hd ks = (accessor (hd vs))
		    then Ss.Yield ((hd vs), (tl ks, tl vs))
		    else Ss.Skip (ks, tl vs))

let subseq start finish ll = S.sub ll start (finish - start + 1)

let ints_from n = iterate n ((+) 1) 

let add_times ?(first_tick=0) genstate_seq =
  map2 make (ints_from first_tick) genstate_seq

let remove_times genstate_seq = map state genstate_seq

let sublist start_time finish_time genstate_seq =
  S.take_while ~f:(fun gs -> gs.time <= finish_time)
                 (S.drop_while ~f:(fun gs -> gs.time < start_time)
		                 genstate_seq)

let select_by_times generations genstate_seq =
  select time generations genstate_seq
