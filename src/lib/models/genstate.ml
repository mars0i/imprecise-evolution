module Mat = Owl.Mat
module Sq = Core.Sequence

let (%) f g = (fun x -> f (g x))

let always _ = true

type genstate = {time : int ; state : Mat.mat list}

type genstate_seq = genstate Sq.t

(** genstate functions *) 

(** accessor, constructor functions: *)
let time gs = gs.time
let state gs = gs.state
let make time state = {time ; state}

(** genstate_seq functions *) 

let hd = Sq.hd_exn
let tl = Sq.tl_eagerly_exn
let cons x xs = Sq.(append (singleton x) xs)
let next = Sq.next
let is_empty = Sq.is_empty
let nth = Sq.nth_exn
let fold_left f init seq = Sq.fold ~f ~init seq
let map f xs = Sq.map ~f xs
let iterate init f = Sq.memoize (Sq.unfold ~init ~f:(fun x -> Some (x, f x)))
let take n seq = Sq.take seq n
let take_while f seq = Sq.take_while ~f seq
let drop n seq = Sq.drop seq n
let drop_while f seq = Sq.drop_while ~f seq
let to_list = Sq.to_list

(* Note Core.Sequence.range doesn't allow an unbounded sequence. *)
let ints ?(stride=1) init_n = iterate init_n (fun n -> n + stride)

(* let map2 f xs ys = Sq.map ~f:(fun (x, y) -> f x y) (Sq.zip xs ys) *)

(* Based on Yaron Minsky's def at https://discuss.ocaml.org/t/how-to-write-map2-for-base-sequence/2090/3?u=mars0i *)
let map2 f s1 s2 =
  Sq.unfold ~init:(s1,s2)
    ~f:(fun (s1,s2) ->
      match Sq.next s1, Sq.next s2 with
      | None, _ | _, None -> None
      | Some (x1, rest1), Some (x2, rest2) -> Some (f x1 x2, (rest1, rest2)))

(**
Example:
{[
    let vals = Sq.zip (Sq.repeat "foo") (Sq.range 0 10)
    let keys = Sq.of_list [2;3;5]
    select snd keys vals2 |> Sq.to_list
    - : (string * int) list = [("foo", 2); ("foo", 3); ("foo", 5)]
]}
*)

let id x = x

(* There's probably a more idiomatic way to do this. *)
let select_in_order is_before accessor keys vals =
  let open Core.Sequence in
  let open Core.Sequence.Step in
  unfold_step ~init:(keys, vals)
    ~f:(fun (ks, vs) -> 
         if is_empty ks || is_empty vs then Done 
         else let k, v = hd_exn ks, hd_exn vs in
              let vtag = accessor v in
              if k = vtag  (* found element in vs; add it to output seq: *)
	      then Yield (k, (tl_eagerly_exn ks, tl_eagerly_exn vs))
              else if is_before k vtag   (* maybe there are gaps in vals *)
              then Skip (ks, tl_eagerly_exn vs)  (* let vs catch up *)
              else Skip (tl_eagerly_exn ks, vs)) (* let ks catch up *)

let select = select_in_order (>)

let subseq start finish ll = Sq.sub ll start (finish - start + 1)

let ints_from n = iterate n ((+) 1) 

let add_times ?(first_tick=0) genstates =
  map2 make (ints_from first_tick) genstates

let remove_times genstates = map state genstates

let sublist start_time finish_time genstates =
  Sq.take_while ~f:(fun gs -> gs.time <= finish_time)
                 (Sq.drop_while ~f:(fun gs -> gs.time < start_time)
		                 genstates)

let select_by_times generations genstates =
  select time generations genstates
