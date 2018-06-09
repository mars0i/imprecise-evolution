(** Models.Seq *)
module S = Core.Sequence


type 'a t = 'a S.t

val hd : 'a S.t -> 'a
val tl : 'a S.t -> 'a S.t
val next : 'a S.t -> ('a * 'a S.t) option
val is_empty : 'a S.t -> bool
val nth : 'a S.t -> int -> 'a
val cons : 'a -> 'a S.t -> 'a S.t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b S.t -> 'a

val memoize : 'a S.t -> 'a S.t

(** [map_raw f xs] maps function [f] over sequence [xs] without memoizing
    the results of the mapping. That is, [f] will be reapplied every
    time an element resulting sequence is subsequently evaluated. *)
val map_raw : ('a -> 'b) -> 'a S.t -> 'b S.t

(** [map f xs] maps function [f] over sequence [xs], memoizing the result 
    of the mapping.  That is, [f] will not be reapplied to elements that
    have previously been evaluated. *)
val map : ('a -> 'b) -> 'a S.t -> 'b S.t

(** [map2_raw f xs1 xs2] is like [map_raw], but [f] is applied to
    corresponding pairs of elements of [xs1] and [xs2]. *)
val map2_raw : ('a -> 'b -> 'c) -> 'a S.t -> 'b S.t -> 'c S.t

(** [map2 f xs1 xs2] is like [map], but [f] is applied to
    corresponding pairs of elements of [xs1] and [xs2]. *)
val map2 : ('a -> 'b -> 'c) -> 'a S.t -> 'b S.t -> 'c S.t

(** [iterate init f] generates a sequence [init, f init, f (f init), ...]
    without memoizing the results of the applications. That is, elements
    of sequence will be recomputed every time the are ievaluated. *)
val iterate_raw : 'a -> ('a -> 'a) -> 'a S.t

(** [iterate init f] generates a sequence [init, f init, f (f init), ...],
    memoizing the results of the applications. That is, elements of the
    sequence will be NOT be recomputed every time they are evaluated. *)
val iterate : 'a -> ('a -> 'a) -> 'a S.t

val take : int -> 'a S.t -> 'a S.t
val take_while : ('a -> bool) -> 'a S.t -> 'a S.t
val drop : int -> 'a S.t -> 'a S.t
val drop_while : ('a -> bool) -> 'a S.t -> 'a S.t
val to_list : 'a S.t -> 'a list

(* [range_raw n m] returns a sequence of integers from [n] to [m]
   inclusive, where each subsequent integer is [stride] plus the
   previous one (default: 1).  Elements sequence will be recomputed 
   every time they are evaluated. *)
val range_raw : ?stride:int -> int -> int -> int S.t

(* [range n m] returns a sequence of integers from [n] to [m]
   inclusive, where each subsequent integer is [stride] plus the
   previous one (default: 1).  Elements of the sequence will NOT 
   be recomputed every time. *)
val range : ?stride:int -> int -> int -> int S.t

(* Note Core.Sequence.range doesn't allow an unbounded sequence. *)
(** [ints_raw ~skip:n init_n] returns an infinite sequence of
    integers [~skip] apart starting from [init_n].  [skip]
    defaults to 1.  Giving [stride] a negative value will produce a
    descending sequence.  Elements of the sequence will be recomputed 
    every time they are evaluated. *)
val ints_raw : ?stride:int -> int -> int S.t

(** Like [ints], but existing results will not be recomputed. *)
val ints : ?stride:int -> int -> int S.t

(** Return a lazy list that's a sublist of the argument, from element start 
    (zero-based) to element finish, inclusive. *)
val subseq : int -> int -> 'a S.t -> 'a S.t

(** 
In [select_in_order is_before accessor keys data], [keys] and [data] are lazy
sequences.  The function returns a lazy sequence of elements from [data]
such that [accessor val] is equal ([=]) to some element [keys].  Both 
keys and the values by which elements of [data] are selected must be numbers
and ordered so that they change monotonically according to boolean comparison 
function [is_before]
[>].  Example:
{[
    let vals = S.zip (S.repeat "foo") (S.range 0 10)
    let keys = S.of_list [2;3;5]
    select (>) snd keys vals2 |> S.to_list
    - : (string * int) list = [("foo", 2); ("foo", 3); ("foo", 5)]
]}
(This function roughly does what a hashtable or map can do, but doesn't 
require the data structures needed for arbitary lookups.)
*)
val select_in_order : ('a -> 'a -> bool) -> ('b -> 'a) -> 'a S.t -> 'b S.t -> 'b S.t

(** Identity function. *)
val id : 'a -> 'a

