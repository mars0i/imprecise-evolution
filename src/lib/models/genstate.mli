module Mat = Owl.Mat
module L = Batteries.List
module LL = Batteries.LazyList

(** Function that always returns [true] given any argument. *)
val always : 'a -> bool

type genstate = {time : int; state : Mat.mat list; }
type genstate_seq = genstate LL.t

(** genstate functions *) 

(** accessor, constructor functions: *)

val time : genstate -> int
val state : genstate -> Mat.mat list
val make : int -> Mat.mat list -> genstate

(** tdistslist functions *) 

val hd : 'a LL.t -> 'a
val tl : 'a LL.t -> 'a LL.t
val last : 'a LL.t -> 'a
val next : 'a LL.t -> 'a LL.node_t
val is_empty : 'a LL.t -> bool
val nth : 'a LL.t -> int -> 'a
val cons : 'a -> 'a LL.t -> 'a LL.t
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b LL.t -> 'a
val fold_right : ('a -> 'b Lazy.t -> 'b) -> 'a LL.t -> 'b Lazy.t -> 'b Lazy.t
val map : ('a -> 'b) -> 'a LL.t -> 'b LL.t
val map2 : ('a -> 'b -> 'c) -> 'a LL.t -> 'b LL.t -> 'c LL.t
val from_loop : 'a -> ('a -> 'b * 'a) -> 'b LL.t
val iterate : 'a -> ('a -> 'a) -> 'a LL.t
val take : int -> 'a LL.t -> 'a LL.t
val take_while : ('a -> bool) -> 'a LL.t -> 'a LL.t
val drop : int -> 'a LL.t -> 'a LL.t
val drop_while : ('a -> bool) -> 'a LL.t -> 'a LL.t
val to_list : 'a LL.t -> 'a list
val nil : 'a LL.t
val lazy_range : ?step:int -> int -> int -> int LL.t


(** [lazy_ints ~every:n init_n] returns an infinite sequence of
    integers [~every] apart starting from [init_n].  [every]
    defaults to 1.  Giving it a negative value will produce a
    descending sequence. *)
val lazy_ints : ?skip:int -> int -> int LL.t

(** In [lazy_select accessor keys data], [keys] and [data] are lazy
    lists.  The function returns a lazy list of elements from [data]
    such that [accessor val] is equal to some element [keys].  Both 
    keys and the values by which elements of [data] are selected should
    be monotonically increasing numbers of the same kind, usually [int]s.
    For example, if [keys] and [data] are increasing sequences of integers,
    and [accessor] is the identity function, [lazy_select] returns the
    intersection of [keys] and [data]. *)
val lazy_select : ('a -> 'b) -> 'b LL.t -> 'a LL.t -> 'a LL.t

(** Iteration functions *)

(* Based on Batteries.LazyList.lazy_fold_right *)
(** [lazy_fold_right2 f l1 l2 init_val] folds function [f] over two lazy 
    lists [l1] and [l2], with initial value [init_val].  Note that for
    constructing lazy lists, one must use [Cons] and [nil] rather than
    [cons] and [nil], as with the [LazyList] eager fold functions:
    {[
      let natnos = iterate 0 ((+) 1)
      let posints = iterate 1 ((+) 1)
      let prods = lazy_fold_right2 (fun x y acc -> Cons(x*y, acc)) natnos posints TL.nil;;
      (to_list (take 10 prods));;
      - : int list = [0; 2; 6; 12; 20; 30; 42; 56; 72; 90]
    ]} *)
val lazy_fold_right2 :
  ('a -> 'b -> 'c lazy_t -> 'c) ->
  'a LL.t -> 'b LL.t -> 'c Lazy.t -> 'c lazy_t

(** Return a lazy list that's a sublist of the argument, from element start 
    (zero-based) to element finish, inclusive. *)
val sub_lazy_list : int -> int -> 'a LL.t -> 'a LL.t

(* val lazy_take_at_idxs : int list -> 'a LL.t -> 'a LL.t *)

(** [ints_from n] generates a lazy list of integers starting from [n]. *)
val ints_from : int -> int LL.t

(** [add_times genstate_seq] makes a lazy list of [genstate] from a lazy list
   of lists of state matrices, adding generation numbers in the [gen]
   field.  If [~first_tick] is given, start with that generation number;
   otherwise begin with generation 0. *)
val add_times : ?first_tick:int -> Mat.mat list LL.t -> genstate LL.t

(** Reverse operation of [add_times]: Given a lazy list of [genstate],
    returns a lazy list of lists of state vectors. *)
val remove_times : genstate LL.t -> Mat.mat list LL.t

(** [sublist start_t finish_t genstate_seq] returns a lazy list that's
    a finite sublist of [genstate_seq], from the first element with 
    [gen] >= [start_time] to the last element with [gen] <= [finish_time].  
    Note that if the list is infinite and there are no elements satisfying
    both of these conditions, the function will try to run forever. *)
val sublist : int -> int -> genstate LL.t -> genstate LL.t

(** In [select_by_times generations genstate_seq], [generations] is a lazy
    list of integers in increasing order, and [genstate_seq] is a lazy
    list of genstate.  The function returns a lazy list contanining those 
    genstate whose generation numbers match the integers in [generations]. *)
val select_by_times : int LL.t -> genstate LL.t -> genstate LL.t
