open Core.Std
open WorldObjectI


(*************************)
(***** Configuration *****)
(*************************)

(** The icon width for each position. *)
let obj_width : int = 10

(** The icon height for each position. *)
let obj_height : int = 10

(** The world has size x size positions *)
let size : int = 67

(** A random number generator for the world.  All random events happening in
   the world should use this random number generator. *)
let rand : int -> int = Random.self_init () ; Random.int

(***********************)
(***** World State *****)
(***********************)

(** The state of the world -- a matrix of positions, where each
   position contains a list of world objects. *)
let world : world_object_i list array array =
  Array.make_matrix ~dimx:size ~dimy:size []

(****************************)
(***** World Operations *****)
(****************************)

(** Clear out all objects from the world. *)
let reset () : unit =
  Array.iter ~f:(fun row -> Helpers.array_map_modify (fun _ -> []) row)
             world

(** Get all objects associated with a location in the world. *)
let get ((x,y):int*int) : world_object_i list =
  world.(x).(y)

(** Set a location in the world to contain a new list of objects. *)
let set ((x,y):int*int) (wos:world_object_i list) : unit =
  world.(x).(y) <- wos

(** Modify a location in the world with value os to contain (f os). *)
let modify (p:int*int) (f:world_object_i list -> world_object_i list) : unit =
  set p (f (get p))

(** Add an object to the list of world objects at a location. *)
let add (p:int*int) (w:world_object_i) : unit =
  modify p (fun wos -> if List.mem wos w then wos else w::wos)


(** Remove an object from the list of world objects at a location. Does
    nothing if the object was not in the list. *)
let remove (p:int*int) (w:world_object_i) : unit =
  modify p (fun wos -> List.filter ~f:(fun w' -> w' <> w) wos)

(** Remove all object from the list of world objects at a location. Does
    nothing if the object was not in the list. *)
let remove_xy (p:int*int) : unit =
  modify p (fun _ -> [])


(** Same as remove but fails if the object is not in the list. *)
let remove_must_exist (p:int*int) (w:world_object_i) : unit =
  assert (List.mem (get p) w) ;
  remove p w

(** Fold over all objects in the world. *)
let fold (f:world_object_i -> 'a -> 'a) (i:'a) : 'a =
  Array.fold_right
    ~f:(fun row accum ->
       Array.fold_right
         ~f:(fun os accum' -> List.fold_right ~f ~init:accum' os)
         ~init:accum
         row)
    ~init:i
    world

(** Call a function for all indices in the world. *)
let indices (f:int*int -> unit) : unit =
  Array.iteri ~f:(fun x -> Array.iteri ~f:(fun y _ -> f (x,y))) world

(** True if the world contains the point (x,y). *)
let check_bounds ((x,y):int*int) : bool =
  x >= 0 && x < size && y >= 0 && y < size

(** Iterate of all world objects along with their corresponding location. *)
let iteri (f:int*int -> world_object_i -> unit) : unit =
  indices (fun p -> List.iter ~f:(f p) (get p))

(** True if the world contains no objects at point p. *)
let is_empty p = get p = []
