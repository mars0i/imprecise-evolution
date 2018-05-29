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
val rev : 'a LL.t -> 'a LL.t


(** [lazy_ints ~skip:n init_n] returns an infinite sequence of
    integers [~skip] apart starting from [init_n].  [skip]
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

(** Convenience abbreviation for [LazyList.(to_list (take n lazy_list))] *)
val take2list : int -> 'a LL.t -> 'a list

(** Return a lazy list that's a sublist of the argument, from element start 
    (zero-based) to element finish, inclusive. *)
val sub_lazy_list : int -> int -> 'a LL.t -> 'a LL.t

(** Convenience function: Takes elements from start to finish, inclusive, 
    from a LazyList, and convert the result to a List. *)
val take_to_list : int -> int -> 'a LL.t -> 'a list

(** Create a power set of integers from a smaller power set.
    Given a sequence of sequences representing the power set of non-negative
    integers up through n-1, returns a pair consisting of the sequence of 
    sequences for up through n-1 and one up through n.  (This kind of pair 
    is what LazyList.from_loop wants.)  Assumes that the first element of 
    the first element of the power set sequence pset passed in is n-1. *)
val next_intsets : int list list -> int list list * int list list

(** Generate a list of subsequent integer power sets. 
    Return a lazy list of subsequent power sets of integers from 0 to n. 
    They can be retreived using e.g., to get the power set of integers
    up to 5: LazyList.at intsets 4 .  Note each power set is in the form
    of a regular list; only the top level list is lazy. *)
val make_intsets : unit -> int list list LL.t

(** A lazy list of integer power sets. *)
val algebra_sets : int list list LL.t

(*********** probabilities over algebras **********)

(** Set difference for ordered lists of integers.
    Given lists xs and ys that are both ordered in the same way (e.g. 
    monotonically ordered integers), return a list containing all
    elements of xs not in ys (with order preserved).  The elements
    in ys must be a (perhaps improper) subset of xs.
    This version by RichN at https://codereview.stackexchange.com/a/162407/61384 *)
val subtract_list : 'a list -> 'a list -> 'a list

(** Set complement for ordered lists of integers.
    i.e. return the atoms representing the negation of the original set.
    Given a maximum element omega_max and a subset of a domain of atoms 
    represented by list of integers in decreasing order, return a list
    of the integers, between 0 and omega_max inclusive, that are not in
    the subset.  The returned list will also be in decreasing order.
    Note that omega_max is one less than the size of the domain. *)
val list_complement : int -> int list -> int list

(** Given a vector of atom probabilities and a list of indexes representing
    atoms, returns sum of probabilities for the set containing those atoms. *)
val prob_sum : Mat.mat -> int list -> float

(** Return (1 - the sum of probs of extreme probs) for a set of atoms
    See (3) and (4) in Skulj. *)
val invert_prob_sum : int -> Mat.mat -> int list -> float

(** Given a probability vector, returns an alist of pairs for all
    elements in the algebra of sets based on the atoms represented by
    elements in the vector.  Each pair contains a list of indexes 
    representing atoms in a set followed by the probability of that set. *)
val algebra_probs : Mat.mat -> (int list * float) list

(** Map prob_sum over each possible combination of atoms. *)
val simple_sums : int -> Mat.mat -> float list

(** Map invert_prob_sum over each possible combination of atoms. *)
val inverted_sums : int -> Mat.mat -> float list

(*
(** Calculate L values for all members of the algebra and return an
    (atoms, L-value) alist.  See (3) in Skulj. *)
val pri_f_field_lowers : int -> Mat.mat -> Mat.mat -> (int list * float) list

(** Calculate U values for all members of the algebra and return an
    (atoms, U-value) alist.  See (4) in Skulj. *)
val pri_f_field_uppers : int -> Mat.mat -> Mat.mat -> (int list * float) list
*)

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
