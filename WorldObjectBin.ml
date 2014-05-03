open Core.Std
open BinI

class type wo_bin_i =
object
  (** Insert the bin list in world object. *)
  method push : (BinI.bin list) -> unit

  (** Reset all the element link to this block in the WorldBin. *)
  method reset : unit -> unit

  (** How to draw the object. *)
  method draw : unit

  (** For Text Plot of efficiency calculation on world. *)
  method show : WorldBins.algotype -> unit

end

class wo_bins_block : wo_bin_i =
object (self)

  val mutable blist:bin list = []
  val mutable coord_extreme_pair:(int * int * bin) list = []
  val mutable coord_initial_pair:(int * int * bin) list = []
  (* maximum height of single plot on particular x-axis. *)
  val mutable maximum_y = 0
  val mutable current_y = 0
  val mutable current_x = 0

(*  initializer
    self#draw
 *)

  method private get_extrems () =
    let rec aux ms =
      match ms with
      | [] -> ()
      | m :: tl -> 
	 let ns = m#get_coords () in
	 coord_extreme_pair <- 
	   (Helpers.cood_x ns Int.max, Helpers.cood_y ns Int.max, m) ::
	     coord_extreme_pair;
	aux tl
    in
    aux blist


  (* print in single row by row. *)
  method private set_initial_coords () =
    let rec aux ns =
      match ns with
      | [] -> ()
      | (x, y, b) :: ns' ->
	 begin
	   (* check x-axis extreme edge of bin *)
	   if not (World.check_bounds (current_x + x + 1, 0)) 
	   then(
	     current_x <- 0;
	     current_y <- current_y + maximum_y + 2)
	   else
	     (if y > maximum_y then maximum_y <- y);
	 end;
	 coord_initial_pair <- (current_x + 1, current_y + 1, b) ::
				 coord_initial_pair;
	 current_x <- current_x + x + 2;
	 aux ns'
    in
    aux (Helpers.sort_on_y coord_extreme_pair)
   
  method push (x: BinI.bin list) : unit =
    blist <- x @ blist
			
  method draw =
    self#get_extrems ();
    self#set_initial_coords ();    
    List.iter ~f:(fun (x, y, b) -> 
		  let wob = new WorldObjectBlock.wo_block (x,y) in
		  b#set_block_disp wob;
		  b#draw;
		  wob#draw) coord_initial_pair

  method show (t:WorldBins.algotype) : unit  =
    let obj = new BinAverage.bin_avg in
    obj#calc_pack blist t

  method reset () =
    let rec aux bl =
      match bl with
      | [] -> ()
      | (x, y, _) :: tl -> World.remove_xy (x,y); aux tl
    in
    aux coord_initial_pair;
    coord_initial_pair <- [];
    ()

end
