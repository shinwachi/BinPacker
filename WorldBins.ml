open Core.Std
open BinI

type algotype = FFD | FF | BF | Nothing

let args_option = ref 10
let count = ref 0
let mark_r = ref 0
let mark_l = ref 0
let binslist : (bin list * algotype) list ref = ref []
let timecapture : (algotype * float) list ref = ref []


(** Clear out all objects from the world. *)
let reset () : unit =
  World.reset ();
  binslist := [];
  timecapture := [];
  mark_r := 0;
  mark_l := 0;
  count := 0;
  ()

(** Add bin list to contain a new list of objects. *)
let add (b:bin list) (a:algotype) : unit =
  incr count;
  !binslist <- (b,a) :: !binslist
		
(** get bin list to right side of binslist. *)			
let get_right () : (bin list * algotype) option =
  if 0 <= !mark_r && !mark_r < !count
  then(
    match !binslist with
    | [] -> None
    | _ -> 
       !mark_l <- !mark_r;
       let x = List.nth !binslist !mark_r in
       incr mark_r; x) 
  else
    None

(** get bin list to left side of binslist. *)			
let get_left () : (bin list * algotype) option =
  if !mark_l < !count && !mark_l > 0
  then(
    match !binslist with
    | [] -> None
    | _ -> 
       !mark_r <- !mark_l;
       decr mark_l;
       List.nth !binslist !mark_l)
  else
    None
