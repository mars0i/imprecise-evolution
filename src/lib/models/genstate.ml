(** Models.Genstate *)

module Mat = Owl.Mat
module Seq = Utils.Seq

(* open Bin_prot.Std *)   (* for @@deriving bin_prot *) 
(* open Bin_prot.Common *) (* for @@deriving bin_prot *)

type t = {time : int ; state : Mat.mat list} (* [@@deriving bin_io] *)

type genstate_seq = t Seq.t

(** accessor, constructor functions: *)
let time gs = gs.time
let state gs = gs.state
let make time state = {time ; state}

let select_by_times generations genstates =
  Seq.select_in_order (>) time generations genstates

let add_times ?(first_tick=0) genstates =
  Seq.map2 make (Seq.ints first_tick) genstates

let remove_times genstates = Seq.map state genstates

let sublist start_time finish_time genstates =
  Seq.take_while (fun gs -> gs.time <= finish_time)
                 (Seq.drop_while (fun gs -> gs.time < start_time)
		                 genstates)
