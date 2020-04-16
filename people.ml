(*
                          CS 51 Problem Set
                 Simulation of an Infectious Disease

                      People in the simulation
 *)

module G = Graphics ;;
open Config ;;
open Registry ;;
open Utilities;;
module Ctr = Counter ;;
module Viz = Visualization ;;
module Stat = Statistics ;; 
(* also uses Utilities *)

(*....................................................................
                                People
 *)
  
class person (initx : int) (inity : int)
             (initstepsize : int)
             (initinfect : float) 
             (initdeadly : float)
              =
  object (self)
    val id : string = Utilities.gensym ()
    val mutable posx : int = initx
    val mutable posy : int = inity
    val mutable step_size : int = initstepsize
    val mutable infectiousness : float = initinfect
    val mutable deadliness : float = initdeadly

                  
    method id : string = id
    method step_size : int = step_size
    method infectiousness : float = infectiousness
    method deadliness : float = deadliness
                  
    method set_pos (x : int) (y : int) : unit =
      posx <- x;
      posy <- y

    method pos = posx, posy
                         
    method set_step_size (new_step_size : int) : unit =
      step_size <- new_step_size
                     
    method set_infectiousness (new_infect : float) : unit =
      infectiousness <- new_infect

    method set_deadliness (new_dead : float) : unit =
      deadliness <- new_dead

    method move : unit =
      let x, y = self#pos in
      let newx, newy =
        Utilities.rand_step x y self#step_size in
      (* drop from old location in registry *)
      Registry.deregister (self :> thing_type);
      (* update location *)
      self#set_pos newx newy;
      (* re-add at the new location *)
      Registry.register (self :> thing_type)

    method update : unit =
      self#move
  
    method draw : unit =
      let x, y = self#pos in
      Viz.draw_circle x y G.black
  end ;;

(*....................................................................
                       People in various states

  Note that since these classes refer to each other, they must be
  simultaneously defined using `and` instead of sequentially defined
  as separate classes.  
 *)
  
class susceptible (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_SUSCEPTIBLE
                   cINFECTIOUSNESS_SUSCEPTIBLE
                   cDEADLINESS_SUSCEPTIBLE
            as super

    initializer
      Stat.susceptible#bump

    method! update =
      super#update;
      let posx, posy = self#pos in
      let infectiousness_total =
        (* calculate total infectiousness of all neighbors *)
        Utilities.sum_float
	  (List.map (fun obj -> obj#infectiousness)
                    (Registry.neighbors (self :> thing_type))) in
      let deadliness_total =     
            Utilities.sum_float
    (List.map (fun obj -> obj#deadliness)
                    (Registry.neighbors (self :> thing_type))) in
      if Utilities.flip_coin infectiousness_total then
        (* infected, so update the registry by replacing this object
           with an infected one *)
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new infected posx posy) :> thing_type)
        end
      else if Utilities.flip_coin deadliness_total then 
        begin
          Stat.susceptible#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new zombie posx posy) :> thing_type)
        end

    method! draw =
      let x, y = self#pos in
      Viz.draw_circle x y cCOLOR_SUSCEPTIBLE
  end









and (* class *) infected (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_INFECTED
                   cINFECTIOUSNESS_INFECTED 
                   cDEADLINESS_INFECTED

            as super    

    initializer
      Stat.infected#bump
      val mutable recoverTime = gaussian (fst(cRECOVERY_PERIOD)) (snd(cRECOVERY_PERIOD))



    method! draw =
      let x, y = self#pos in 
      Viz.draw_circle ~size:10 ~filled:false x y cCOLOR_INFECTED;
      Viz.draw_circle x y cCOLOR_INFECTED

    method! update =
      super#update;
      recoverTime <- (recoverTime -. 1.);
      let posx, posy = self#pos in
      if recoverTime < 0. then
        begin
          Stat.infected#debump;
          Registry.deregister (self :> thing_type);
          if (Random.float(1.) > cMORTALITY) then
            Registry.register ((new recovered posx posy) :> thing_type)
          else
            Registry.register ((new deceased posx posy) :> thing_type)
        end


  end




and (* class *) recovered (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_RECOVERED
                   cINFECTIOUSNESS_RECOVERED
                   cDEADLINESS_RECOVERED
            as super

    initializer
      Stat.recovered#bump
      val mutable immuneTime = gaussian (fst(cIMMUNITY_PERIOD)) (snd(cIMMUNITY_PERIOD))


    method! draw =
      let x, y = self#pos in 
      Viz.draw_circle x y cCOLOR_RECOVERED

    method! update =
      super#update;
      immuneTime <- (immuneTime -. 1.);
      let posx, posy = self#pos in
      if immuneTime < 0. then
        begin
          Stat.recovered#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new susceptible posx posy) :> thing_type)
        end

  end


and (* class *) deceased (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_DECEASED
                   cINFECTIOUSNESS_DECEASED
                   cDEADLINESS_DECEASED
            as super

    initializer
      Stat.deceased#bump


    method! draw =
      let x, y = self#pos in 
      Viz.draw_cross x y cCOLOR_DECEASED

    method! update =
      super#update;
      let posx, posy = self#pos in
      if (Random.float(1.) > cZOMBIENESS) then
        begin
          Stat.deceased#debump;
          Registry.deregister (self :> thing_type);
          Registry.register ((new zombie posx posy) :> thing_type)
        end

  end

and (* class *) zombie (initx : int) (inity : int) =
  object (self)
    inherit person initx inity
                   cSTEP_SIZE_ZOMBIES
                   cINFECTIOUSNESS_ZOMBIES
                   cDEADLINESS_ZOMBIES
            as super

    initializer
      Stat.zombies#bump


    method! draw =
      let x, y = self#pos in 
      Viz.draw_circle ~size:10 x y cCOLOR_ZOMBIE2;
      Viz.draw_cross x y cCOLOR_ZOMBIE


  end






(*....................................................................
Place definitions for any other classes here. In particular, you'll
want to at least implement a `recovered` class for `infected` people
who have recovered from the infection.
....................................................................*)
;;
