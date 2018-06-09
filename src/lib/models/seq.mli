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
val map : ('a -> 'b) -> 'a S.t -> 'b S.t
val map2 : ('a -> 'b -> 'c) -> 'a S.t -> 'b S.t -> 'c S.t
val iterate : 'a -> ('a -> 'a) -> 'a S.t
val take : int -> 'a S.t -> 'a S.t
val take_while : ('a -> bool) -> 'a S.t -> 'a S.t
val drop : int -> 'a S.t -> 'a S.t
val drop_while : ('a -> bool) -> 'a S.t -> 'a S.t
val to_list : 'a S.t -> 'a list

val range : ?stride:int -> int -> int -> int S.t

(* Note Core.Sequence.range doesn't allow an unbounded sequence. *)
(** [ints ~skip:n init_n] returns an infinite sequence of
    integers [~skip] apart starting from [init_n].  [skip]
    defaults to 1.  Giving it a negative value will produce a
    descending sequence. *)
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

