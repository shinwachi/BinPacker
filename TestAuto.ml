open Core.Std

open BinI
open GeneratorI
open BinGenI
open PackerI
open Test
(* testing framework *)

exception Stop

let testing_auto () : unit =
  WorldBins.reset ();
  Graphics.clear_graph ();
  let str : string = "Please wait.. In Progress..." in
  let wotxt = new WorldObjectText.world_object_text str (18,35) Graphics.red in
  wotxt#draw;

  let open World in
  let f1 x = if x < 5 then 5 else x in
  let bin_height = f1 (rand 20) in
  let bin_width = f1 (rand 15) in
  
  let f2 x = if x < 50 then 50 else x in
  let p_count = f2 (rand 100) in
  let p_height = Helpers.with_inv_probability_or rand 3
		 (fun () -> bin_height - 1) 
		 (fun () -> bin_height/2) in
  let p_width = Helpers.with_inv_probability_or rand 3
		(fun () -> bin_width - 1) 
		(fun () -> bin_width/2) in
  let bg =
    Helpers.with_inv_probability_or World.rand 2 
	  (fun () ->  new bingen_rect (bin_width, bin_height))
	  (fun () ->  new bingen_rect_hole (bin_width, bin_height))
  in
  let pg = new generator_random p_count (p_width, p_height) in
  let x = new ff_packer in
  let y = new ffd_packer in
  let z = new bf_packer in
  let pt = new packer_test pg bg [x;y;z] in
  pt#test_packers ();
  UI.button_press_plot_right ();
  UI.button_press_plot_right ();
  UI.button_press_plot_right ();
  UI.terminate ();
  ()
