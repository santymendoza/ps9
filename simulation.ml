(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                        The simulation driver
 *)

open Config ;;
module Stat = Statistics ;; 
module T = Registry ;;
module P = People ;;
module R = T.Registry ;;
module Viz = Visualization ;;
  
let run () =
  Viz.initialize ();

  (* track the counts of each type of person, one count-list per time
     step, in reverse temporal order *)
  let counts : int list list ref =
    ref [] in
               
  (* initialize the population with some susceptible and infected
     people *)
  for _i = 1 to cPOPULATION do
    let x, y = Random.int cX_DIMENSION, Random.int cY_DIMENSION in
    if Random.float 1.0 < cINITPROPORTION then
      R.register (new P.infected x y :> T.thing_type)
    else
        R.register (new P.susceptible x y :> T.thing_type)
  done;

  (* repeatedly update all the members of the population *)
  let leave_early = ref false in
  while Stat.time#count < cTIME_STEPS && not !leave_early do
    Stat.time#bump;

    (* pause every so often *)
    if Stat.time#count mod cPAUSE_EVERY = 0 then
      Viz.await_key ();
    
    (* update all the entities *)
    R.registrants ()
    |> List.iter (fun obj -> obj#update);

    (* record the updated counts, in order of the stacked bar chart
       display from bottom to top *)
    counts := [ Stat.deceased#count;
                Stat.infected#count;
                Stat.susceptible#count;
                Stat.recovered#count;
                Stat.zombies#count ]
              :: !counts;

    (* update the display; note that the order of colors matches the
       order of the counts *)
    Viz.render (R.registrants ())
               (List.rev !counts)
               [ cCOLOR_DECEASED;
                 cCOLOR_INFECTED;
                 cCOLOR_SUSCEPTIBLE;
                 cCOLOR_RECOVERED;
                 cCOLOR_ZOMBIE; ];

    (* check to see if a key was pressed to stop the simulation *)
    if Viz.any_key () then leave_early := true
  done;

  (* simulation complete; await a keypress *)
  if not !leave_early then
    Viz.await_key () ;;
