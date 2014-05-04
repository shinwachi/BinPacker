open Core.Std

open BinI
open GeneratorI
open BinGenI
open PackerI
(* testing framework *)

(* should take:
 * generator and bingen
 * packer list
 *)

class testclass (x:int) = 
object
  method getx () = x
end


class type packer_test_i =
object
  method test_packers : unit -> unit
end

class packer_test (gen_init: generator) 
                  (bgen_init: bingen) 
                  (pkgen_init: ((bingen -> generator -> packer) list))
                  : packer_test_i = 
object (self)
  val test_pobjs = gen_init#get_remaining ()
  val test_bingen = bgen_init
  val packers = pkgen_init

  method private get_generator () = new generator_pobj_lst test_pobjs

  method private draw (bs:bin list) (al:WorldBins.algotype) : unit =
    WorldBins.add bs al

  method private progress (alg:WorldBins.algotype) (tm:float) : unit =
    let f (x:string) (y:int*int) z : WorldObjectI.world_object_i = 
      new WorldObjectText.world_object_text x y z in
    match alg with
    | FF ->
       let str = "FF Packing Completed in " ^ Float.to_string tm ^ "s"in
       let wotxt = f str (18,26) Graphics.blue in
       wotxt#draw
    | FFD ->
       let str = "FFD Packing Completed in " ^ Float.to_string tm ^ "s" in
       let wotxt = f str (18,23) Graphics.blue in
       wotxt#draw
    | BF -> 
       let str = "BF Packing Completed in " ^ Float.to_string tm ^ "s" in
       let wotxt = f str (18,20) Graphics.blue in
       wotxt#draw
    | Nothing ->
       World.remove_xy (18,35);
       let str = "Done.. Proceed with -> M or m " in
       let wotxt = f str (18,30) Graphics.green in
       wotxt#draw 
       
  method private test_packer (pkgen:(bingen -> generator -> packer)) : unit =
    let startt = Unix.time () in
    let pkr =  pkgen test_bingen (self#get_generator ()) in
    (* pack bin *)
    pkr#pack_bins ();
    (*get algo type *)
    let name = pkr#get_name () in
    (* retrieve packed bins *)
    let pkrlst = pkr#get_bins () in
    self#draw pkrlst name;
    let endt = Unix.time () in
    let diff = (endt -. startt) in
    self#progress name diff;
    !WorldBins.timecapture <- (name,diff) :: !WorldBins.timecapture;
    ()
    
  method test_packers () : unit =
    (*FOR TESTING: to store all instances, remove "WorldBins.reset ()" *)
    let rec tpkrs pkglst =
      match pkglst with
      | [] -> ()
      | pkg :: pkglst' ->
         self#test_packer pkg;
	 tpkrs pkglst'
    in
    tpkrs packers;
    self#progress Nothing 0.0

end

let testing (opt:int) : unit =
  print_string (" ##RECTANGLE##"); flush_all ();
  WorldBins.reset ();
  Graphics.clear_graph ();
  let str : string = "Please wait.. In Progress..." in
  let wotxt = new WorldObjectText.world_object_text str (18,35) Graphics.red in
  wotxt#draw;
  let open World in
  let f1 x = if x < 5 then 5 else x in
(*  let bin_height = f1( rand 65 + 1) in
  let bin_width = rand ((65-bin_height) + 1) in*)
  let bin_height = f1 (rand 20) in
  let bin_width = f1 (rand 15) in
  (*let f2 x = if x < 50 then 50 else x in*)
  let p_count = 50 (*rand 150*) in
  let p_height = Helpers.with_inv_probability_or rand 3
		 (fun () -> bin_height - 1) 
		 (fun () -> bin_height/2) in
  let p_width = Helpers.with_inv_probability_or rand 3
		(fun () -> bin_width - 1) 
		(fun () -> bin_width/2) in
  let bg =
    match opt with
    | 1 -> new bingen_rect (bin_width, bin_height)
    | 2 -> new bingen_rect_hole (bin_width, bin_height)
    | _ -> failwith "Invalid Option"
  in
  let pg = new generator_random p_count (p_width, p_height) in
  let x = new ff_packer in
  let y = new ffd_packer in
  let z = new bf_packer in
  let pt = new packer_test pg bg [x;y;z] in
  pt#test_packers ();
  ()

let test_tetrominos () : unit = 
  print_string (" ##TETROMINOS##"); flush_all ();
  WorldBins.reset ();
  Graphics.clear_graph ();
  let str : string = "Please wait.. In Progress..." in
  let wotxt = new WorldObjectText.world_object_text str (18,35) Graphics.red in
  wotxt#draw;
  let open World in
  let f1 x = if x < 5 then 5 else x in
  let bin_height = f1 (rand 20) in
  let bin_width = f1 (rand 15) in
  (*let f2 x = if x < 50 then 50 else x in*)
  let p_count = 50 (*rand 150*) in
  let bg = new bingen_rect (bin_width, bin_height) in
  let pg = new generator_tetrominos p_count in
  let x = new ff_packer in
  let y = new ffd_packer in
  let z = new bf_packer in
  let pt = new packer_test pg bg [x;y;z] in
  pt#test_packers ();
  ()


let demo (_:int) : unit =
  print_string (" ##DEMO##"); flush_all ();
  WorldBins.reset ();
  Graphics.clear_graph ();
  let str : string = "Please wait.. In Progress..." in
  let wotxt = new WorldObjectText.world_object_text str (18,35) Graphics.red in
  wotxt#draw;
  let open World in
  let f1 x = if x < 5 then 5 else x in
  let bin_height = f1 (rand 20) in
  let y = if bin_height > 10 then 5 else bin_height in
  let bin_width = f1 (rand y) in
  let p_count = 50 in
  let p_height = Helpers.with_inv_probability_or rand 3
		 (fun () -> bin_height - 1) 
		 (fun () -> bin_height/2) in
  let p_width = Helpers.with_inv_probability_or rand 3
		(fun () -> bin_width - 1) 
		(fun () -> bin_width/2) in
  let bg = new bingen_rect (bin_width, bin_height) in
  let pg = new generator_random p_count (p_width, p_height) in
  let x = new ff_packer in
  let y = new ffd_packer in
  let z = new bf_packer in
  let pt = new packer_test pg bg [x;y;z] in
  pt#test_packers ();
  ()
