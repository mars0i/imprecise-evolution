module Mat = Owl.Mat
module Sq = Core.Sequence

(** Function that always returns [true] given any argument. *)
val always : 'a -> bool

type genstate = {time : int; state : Mat.mat list; }
type genstate_seq = genstate Sq.t

(** genstate functions *) 

(** accessor, constructor functions: *)

val time : genstate -> int
val state : genstate -> Mat.mat list
val make : int -> Mat.mat list -> genstate

val hd : 'a Sq.t -> 'a
val tl : 'a Sq.t -> 'a Sq.t
val next : 'a Sq.t -> ('a * 'a Sq.t) option
val is_empty : 'a Sq.t -> bool
val nth : 'a Sq.t -> int -> 'a
val cons : 'a -> 'a Sq.t -> 'a Sq.t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b Sq.t -> 'a
val map : ('a -> 'b) -> 'a Sq.t -> 'b Sq.t
val map2 : ('a -> 'b -> 'c) -> 'a Sq.t -> 'b Sq.t -> 'c Sq.t
val iterate : 'a -> ('a -> 'a) -> 'a Sq.t
val take : int -> 'a Sq.t -> 'a Sq.t
val take_while : ('a -> bool) -> 'a Sq.t -> 'a Sq.t
val drop : int -> 'a Sq.t -> 'a Sq.t
val drop_while : ('a -> bool) -> 'a Sq.t -> 'a Sq.t
val to_list : 'a Sq.t -> 'a list


(** [ints ~skip:n init_n] returns an infinite sequence of
    integers [~skip] apart starting from [init_n].  [skip]
    defaults to 1.  Giving it a negative value will produce a
    descending sequence. *)
val ints : ?stride:int -> int -> int Sq.t

(** 
In [select_in_order is_before accessor keys data], [keys] and [data] are lazy
sequences.  The function returns a lazy sequence of elements from [data]
such that [accessor val] is equal ([=]) to some element [keys].  Both 
keys and the values by which elements of [data] are selected must be numbers
and ordered so that they change monotonically according to boolean comparison 
function [is_before]
[>].  Example:
{[
    let vals = Sq.zip (Sq.repeat "foo") (Sq.range 0 10)
    let keys = Sq.of_list [2;3;5]
    select (>) snd keys vals2 |> Sq.to_list
    - : (string * int) list = [("foo", 2); ("foo", 3); ("foo", 5)]
]}
*)
val select_in_order : ('a -> 'a -> bool) -> ('b -> 'a) -> 'a Sq.t -> 'b Sq.t -> 'b Sq.t

(** 
In [select accessor keys data], [keys] and [data] are lazy
sequences.  The function returns a lazy sequence of elements from [data]
such that [accessor val] is numerically equal to some element [keys].  Both 
keys and the values by which elements of [data] are selected must be 
monotonically increasing.  Example:
{[
    let vals = Sq.zip (Sq.repeat "foo") (Sq.range 0 10)
    let keys = Sq.of_list [2;3;5]
    select snd keys vals2 |> Sq.to_list
    - : (string * int) list = [("foo", 2); ("foo", 3); ("foo", 5)]
]}
*)
val select : ('a -> int) -> int Sq.t -> 'a Sq.t -> 'a Sq.t

(** Identity function (useful with [select]). *)
val id : 'a -> 'a

(** Return a lazy list that's a sublist of the argument, from element start 
    (zero-based) to element finish, inclusive. *)
val subseq : int -> int -> 'a Sq.t -> 'a Sq.t

(** [ints_from n] generates a lazy list of integers starting from [n]. *)
val ints_from : int -> int Sq.t

(** [add_times genstate_seq] makes a lazy list of [genstate] from a lazy list
   of lists of state matrices, adding generation numbers in the [gen]
   field.  If [~first_tick] is given, start with that generation number;
   otherwise begin with generation 0. *)
val add_times : ?first_tick:int -> Mat.mat list Sq.t -> genstate Sq.t

(** Reverse operation of [add_times]: Given a lazy list of [genstate],
    returns a lazy list of lists of state vectors. *)
val remove_times : genstate Sq.t -> Mat.mat list Sq.t

(** [sublist start_t finish_t genstate_seq] returns a lazy list that's
    a finite sublist of [genstate_seq], from the first element with 
    [gen] >= [start_time] to the last element with [gen] <= [finish_time].  
    Note that if the list is infinite and there are no elements satisfying
    both of these conditions, the function will try to run forever. *)
val sublist : int -> int -> genstate Sq.t -> genstate Sq.t

(** In [select_by_times generations genstate_seq], [generations] is a lazy
    list of integers in increasing order, and [genstate_seq] is a lazy
    list of genstates.  The function returns a lazy list contanining those 
    genstate whose generation numbers match the integers in [generations]. *)
val select_by_times : int Sq.t -> genstate Sq.t -> genstate Sq.t

