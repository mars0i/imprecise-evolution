module Mat = Owl.Mat
module S = Core.Sequence

(** Function that always returns [true] given any argument. *)
val always : 'a -> bool

type genstate = {time : int; state : Mat.mat list; }
type genstate_seq = genstate S.t

(** genstate functions *) 

(** accessor, constructor functions: *)

val time : genstate -> int
val state : genstate -> Mat.mat list
val make : int -> Mat.mat list -> genstate

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


(** [ints ~skip:n init_n] returns an infinite sequence of
    integers [~skip] apart starting from [init_n].  [skip]
    defaults to 1.  Giving it a negative value will produce a
    descending sequence. *)
val ints : ?stride:int -> int -> int S.t

(** 
In [select accessor keys data], [keys] and [data] are lazy
sequences.  The function returns a lazy sequence of elements from [data]
such that [accessor val] is equal to some element [keys].  Both 
keys and the values by which elements of [data] are selected should
have the same order in the sense that they both change monotonically
in the same direction.  Example:
{[
    let vals = S.zip (S.repeat "foo") (S.range 0 10)
    let keys = S.of_list [2;3;5]
    select snd keys vals2 |> S.to_list
    - : (string * int) list = [("foo", 2); ("foo", 3); ("foo", 5)]
]}
*)
val select : ('a -> 'b) -> 'b S.t -> 'a S.t -> 'a S.t

(** Return a lazy list that's a sublist of the argument, from element start 
    (zero-based) to element finish, inclusive. *)
val subseq : int -> int -> 'a S.t -> 'a S.t

(** [ints_from n] generates a lazy list of integers starting from [n]. *)
val ints_from : int -> int S.t

(** [add_times genstate_seq] makes a lazy list of [genstate] from a lazy list
   of lists of state matrices, adding generation numbers in the [gen]
   field.  If [~first_tick] is given, start with that generation number;
   otherwise begin with generation 0. *)
val add_times : ?first_tick:int -> Mat.mat list S.t -> genstate S.t

(** Reverse operation of [add_times]: Given a lazy list of [genstate],
    returns a lazy list of lists of state vectors. *)
val remove_times : genstate S.t -> Mat.mat list S.t

(** [sublist start_t finish_t genstate_seq] returns a lazy list that's
    a finite sublist of [genstate_seq], from the first element with 
    [gen] >= [start_time] to the last element with [gen] <= [finish_time].  
    Note that if the list is infinite and there are no elements satisfying
    both of these conditions, the function will try to run forever. *)
val sublist : int -> int -> genstate S.t -> genstate S.t

(** In [select_by_times generations genstate_seq], [generations] is a lazy
    list of integers in increasing order, and [genstate_seq] is a lazy
    list of genstates.  The function returns a lazy list contanining those 
    genstate whose generation numbers match the integers in [generations]. *)
val select_by_times : int S.t -> genstate S.t -> genstate S.t

