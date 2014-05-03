open Core.Std
open Event51
open MatrixI
open WorldObjectBlock
open BinAverage
open MatrixI
open BinGenI
open GeneratorI
open PackerI

(* Function that is called continuously while the simulation is running. *)
let event_loop () : unit =
  Graphics.clear_graph ();
  UI.world_refresh ()

let parse_args () : (unit -> unit) =
  let usage () = Printf.printf "usage: %s argument\n" Sys.argv.(0); exit 1 in
  if Array.length Sys.argv <> 2 then usage ();
  match Sys.argv.(1) with
  | "GUI" -> (fun () -> !WorldBins.args_option <- 1;
			Test.testing !WorldBins.args_option)
  | "Auto" -> (fun () -> TestAuto.testing_auto ())
  | "Hole" -> (fun () -> !WorldBins.args_option <- 2;
			 Test.testing !WorldBins.args_option)
  | _ -> usage ()

let run () : unit =
  let initialize  = parse_args () in
  UI.run_world initialize event_loop
;;

run () ;;
