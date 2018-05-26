
(* TODO: 
 * Add option to control fill color.
 * Add option to control number of pmap forks.
 *)

module SC = Setchains
module WF = Wrightfisher
module IO = CredalsetIO
module TD = Tdists
module PL = Owl.Plot

let bottom_top_colors=PL.[RGB (0,0,200); RGB (200,0,0)]

(** For use with [Core.Command.basic_spec]. 
    (Keep it in a separate library for debugging in utop.)  *)
let make_setchain_and_save_for_commandline rows cols plot_max fontsize sample
                                           skip updown nofork basename popsize
					   initfreq startgen lastgen fitn_floats () =
  let fitn_recs = WF.group_fitns fitn_floats in  (* parse groups of three floats into separate fitness records *)
  let selected_tdistlists =
    SC.make_setchain_from_fitns ~verbose=true ~fork:(not nofork) ~skip fitn_recs 

  Printf.printf "making pdfs ... \n%!";
  IO.make_setchain_bounds_pdfs ~colors:bottom_top_colors
                ~rows ~cols ~sample_interval:sample ?plot_max ?fontsize ~leftright:(not updown)
                basename selected_tdistlists

