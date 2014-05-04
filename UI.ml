open Core.Std
open Event51
open MatrixI

(********************)
(***** UI State *****)
(********************)

(*let delay = ref 10*)
let delay = ref 0
let counter = ref 0
let inc_counter () =
  incr counter ;
  if !counter >= !delay then (counter := 0 ; true) else false

let mouse_state = ref false
let mouse_pos = ref (0,0)

let paused = ref false
let counter = ref 1

(*********************)
(***** UI Events *****)
(*********************)


(** Fires when a key is pressed and returns the character corresponding
    to the key. *)
let key_pressed : char Event51.event = Event51.new_event ()

(** Fires when the mouse button is pressed, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_down : (int*int) Event51.event = Event51.new_event ()

(** Fires when the mouse button is released, indicating the coordinates
    where the mouse was when the event occurred. *)
let button_up : (int * int) Event51.event = Event51.new_event ()

(** Fires when the mouse moves, indicating the coordinates where the
    mouse was when the event occured. *)
let mouse_motion : (int * int) Event51.event = Event51.new_event ()

(** Fires each time the virtual clock ticks. *)
let clock : unit Event51.event = Event51.new_event ()

(************************)
(***** Event System *****)
(************************)

exception Stop

(* poll the Graphics module for the various events -- some care had to
   be taken to "de-bounce" the mouse. *)
let read_event () =
  let new_pos = Graphics.mouse_pos () in
  if new_pos <> !mouse_pos then begin
    mouse_pos := new_pos ;
    Event51.fire_event mouse_motion (Graphics.mouse_pos ())
  end ;
  if Graphics.key_pressed () then begin
    Event51.fire_event key_pressed (Graphics.read_key ())
  end ;
  if not !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_down ; Graphics.Poll] in
    if s.Graphics.button then begin
      mouse_state := true ;
      Event51.fire_event button_down new_pos
    end
  end ;
  if !mouse_state then begin
    let s = Graphics.wait_next_event [Graphics.Button_up ; Graphics.Poll] in
    if not s.Graphics.button then begin
      mouse_state := false ;
      Event51.fire_event button_up new_pos
    end
  end ;
  Event51.fire_event clock ()

(* Helper for restarting interrupted system calls (OY) *)
let rec restart f arg =
  try f arg
  with Unix.Unix_error (Unix.EINTR, _, _) -> restart f arg

(* The approximate frame rate (will actually be lower if we take non-trivial
   time to handle events) *)
(*let frame_rate = 30.0*)
let frame_rate = 30.0

(* Our basic event loop just calls read_event, which fires the appropriate
   events, then synchronizes the shadow graphics buffer with the screen,
   and then loops again. *)
let rec event_loop () =
  read_event ();
  Graphics.synchronize ();
  restart Thread.delay (1.0 /. frame_rate);
  event_loop ()


(** The command "run_ui x y init" starts up the graphical environment with a
    window size of x by y pixels, sets up the basic events such as the
    keyboard, mouse, etc. (see below), and then invokes the function init as
    an initializer, before entering an event polling loop which fires the
    appropriate event handlers whenever an action occurs. *)
let run_ui (x:int) (y:int) (init:unit->unit) : unit =
  try
    Graphics.open_graph "" ; Graphics.resize_window x y ;
    Graphics.auto_synchronize false ;
    init () ;
    event_loop ()
  with exn -> (Graphics.close_graph () ; raise exn)

(** only call the supplied function on every delay clock ticks and only if the
    simulation is not paused. *)
let clock_handler (f : unit -> unit) () : unit =
  if inc_counter () && not !paused then f ()

(** World Refresh for slow "block" speed. *)
let world_refresh_slow () =
  Graphics.clear_graph ();
  let world_list = World.fold (fun x y -> x :: y) [] in
  let (world_list_n , _) = List.split_n world_list !counter in
  incr counter;
  List.iter ~f:(fun x ->x#draw) world_list_n

(** World Refresh for Normal "block" speed. *)
let world_refresh () =
  Graphics.clear_graph ();
  let world_list = World.fold (fun x y -> x :: y) [] in
  List.iter ~f:(fun x ->x#draw) world_list

(** Help to Plot obejcts in World. *)
let plot (blst :(BinI.bin list * WorldBins.algotype) option) : unit =
  match blst with
  | None -> ()
  | Some x ->
     Graphics.clear_graph ();
     let wob = new WorldObjectBin.wo_bins_block in
     World.reset ();
     wob#push (fst x);
     wob#draw;
     wob#show (snd x)

(** Get lower bound list of object for World. *)
let button_press_plot_left () =
  let bin_list : (BinI.bin list * WorldBins.algotype) option = 
    WorldBins.get_left () in
  plot bin_list;
  ()

(** Get higher bound list of object for World. *)
let button_press_plot_right () =
  let bin_list : (BinI.bin list * WorldBins.algotype) option =
    WorldBins.get_right () in
  plot bin_list;
  ()

(** Exit Function*)
let terminate () : unit =
  Printf.printf " <Termination> "; exit 1; ()

(** Press q or Q to stop the simulation.
    Press space to [un]pause the simulation.
    Press f or F to make the simulation go faster.
    Press s or S to make the simulation go slower. 
    Press n or N to get lower bounds of list of World objects.
    Press m or M to get higher bound of list of World object.
    Press i ir I to ReIntialize World again from start. *)
let key_handler c =
  match c with
    | 'q' | 'Q' -> terminate ()
    | ' ' -> paused := not(!paused)
    | 'f' | 'F' -> delay := (!delay) - 5
    | 's' | 'S' -> delay := (!delay) + 5
    | 'n' | 'N' -> button_press_plot_left ()
    | 'm' | 'M' -> button_press_plot_right ()
    | 'i' | 'I' -> (match !WorldBins.args_option with
		    | 1 | 2 -> Test.testing !WorldBins.args_option
		    | 3 -> Test.test_tetrominos ()
		    | 4 -> Test.demo !WorldBins.args_option
		    | _ -> failwith "Invalid Option")
    | _ -> ()


(** Start the graphical environment initialized to the size of the world.
    Handle clock and input events necessary to run the simulation. *)
let run_world (init:unit -> unit) (clock_f:unit -> unit) : unit =
  run_ui (World.size*World.obj_width) (* GUI width *)
         (World.size*World.obj_height) (* GUI height *)
         (* Event framework initializer *)
         begin fun () ->
           World.reset () ;
           ignore(Event51.add_listener clock (clock_handler clock_f)) ;
           ignore(Event51.add_listener key_pressed key_handler) ;
           init ()
         end
