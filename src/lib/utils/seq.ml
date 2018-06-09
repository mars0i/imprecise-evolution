(** Models.Seq *)
module S = Core.Sequence
module Step = S.Step

type 'a t = 'a S.t

let memoize = S.memoize

let hd = S.hd_exn
let tl = S.tl_eagerly_exn
let cons x xs = S.(append (singleton x) xs) (* Inefficient(?) hack *)
let next = S.next
let is_empty = S.is_empty
let nth = S.nth_exn
let fold_left f init seq = S.fold ~f ~init seq
let map_raw f xs = S.map ~f xs
let map f xs = memoize (map_raw f xs)
let iterate_raw init f = S.unfold ~init ~f:(fun x -> Some (x, f x))
let iterate init f = memoize (iterate_raw init f)
let take n seq = S.take seq n
let take_while f seq = S.take_while ~f seq
let drop n seq = S.drop seq n
let drop_while f seq = S.drop_while ~f seq
let to_list = S.to_list

let range_raw ?(stride=1) first last =
  S.range ~start:`inclusive ~stop:`inclusive ~stride first last

let range ?(stride=1) first last = memoize (range_raw ~stride first last)

(* Note Core.Sequence.range doesn't allow an unbounded sequence. *)
let ints_raw ?(stride=1) init_n = iterate init_n (fun n -> n + stride)
let ints ?(stride=1) init_n = memoize (ints_raw ~stride init_n)

let subseq start finish ll = S.sub ll ~pos:start ~len:(finish - start + 1)

(* Alternative definition: 
let map2_raw f xs ys = S.map ~f:(fun (x, y) -> f x y) (S.zip xs ys) *)
(* Based on Yaron Minsky's def at https://discuss.ocaml.org/t/how-to-write-map2-for-base-sequence/2090/3?u=mars0i *)
let map2_raw f xs1 xs2 =
  S.unfold ~init:(xs1, xs2)
    ~f:(fun (xs1, xs2) ->
      match S.next xs1, S.next xs2 with
      | None, _ | _, None -> None
      | Some (x1, rest1), Some (x2, rest2) -> Some (f x1 x2, (rest1, rest2)))

let map2 f xs1 xs2 = memoize (map2_raw f xs1 xs2)

let id x = x (* for use by select_in_order *)

(* There's probably a more idiomatic way to do this. *)
let select_in_order is_before accessor keys vals =
  S.unfold_step ~init:(keys, vals)
    ~f:(fun (ks, vs) -> 
         if is_empty ks || is_empty vs then Step.Done 
         else let k, v = hd ks, hd vs in
              let vtag = accessor v in (* data from v that we'll compare *)
              if k = vtag  (* found element in vs; add it to output, move on: *)
	      then Step.Yield (v, (tl ks, tl vs))
              else if is_before k vtag   (* maybe there are gaps in vals *)
              then Step.Skip (ks, tl vs)  (* let vs catch up *)
              else Step.Skip (tl ks, vs)) (* let ks catch up *)

