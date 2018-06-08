module Mat = Owl.Mat
(* Uses module Models.Seq *)

type t = {time : int; state : Mat.mat list}
type genstate_seq = t Seq.t

(** accessor, constructor functions: *)
val time : t -> int
val state : t -> Mat.mat list
val make : int -> Mat.mat list -> t

(** [add_times genstate_seq] makes a lazy list of [genstate] from a lazy list
   of lists of state matrices, adding generation numbers in the [gen]
   field.  If [~first_tick] is given, start with that generation number;
   otherwise begin with generation 0. *)
val add_times : ?first_tick:int -> Mat.mat list Seq.t -> t Seq.t

(** Reverse operation of [add_times]: Given a lazy list of [t],
    returns a lazy list of lists of state vectors. *)
val remove_times : t Seq.t -> Mat.mat list Seq.t

(** [sublist start_t finish_t genstate_seq] returns a lazy list that's
    a finite sublist of [genstate_seq], from the first element with 
    [gen] >= [start_time] to the last element with [gen] <= [finish_time].  
    Note that if the list is infinite and there are no elements satisfying
    both of these conditions, the function will try to run forever. *)
val sublist : int -> int -> t Seq.t -> t Seq.t

(** In [select_by_times generations genstate_seq], [generations] is a lazy
    list of integers in increasing order, and [genstate_seq] is a lazy
    list of genstates.  The function returns a lazy list contanining those 
    genstate whose generation numbers match the integers in [generations]. *)
val select_by_times : int Seq.t -> t Seq.t -> t Seq.t
