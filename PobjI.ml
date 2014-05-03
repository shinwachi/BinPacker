open Core.Std
open Helpers
open Direction
open MatrixI
open BoolMatrix

(*open WorldObjectMatrix*)
(*
open WorldObject
 *)
open WorldObjectI
class type pobj_i = 
object
  method print : unit (* ascii rep *)
  method set_location : (int * int) -> unit
  method set_orientation : direction -> unit
  method get_mat : BoolMatrix.matrix
  method get_raw_mat : BoolMatrix.matrix
  method is_equal : pobj_i -> bool
  method overlaps : pobj_i -> bool
  (* gives (absolute, meaning translated and rotated) coordinates *)
  method get_coords : unit -> (int*int) list
  (* clones a new pobj *)
  method clone : unit -> pobj_i
end
  
class pobj (initial_mat:BoolMatrix.matrix) : pobj_i =
object (self)

  val mutable pobjmat : BoolMatrix.matrix = initial_mat
  val mutable location : (int * int ) = (0,0)
  val mutable orientation : direction = Up

  method print = print_string "pobj:\n";
		 print_matrix pobjmat; ()
  method set_location (p:int*int) : unit = location <- p
  method set_orientation (d:direction) : unit = orientation <- d
  method get_mat  =  translate (rotate pobjmat orientation ) location
  method get_raw_mat = pobjmat
  method is_equal (otherpo) =  is_equal (otherpo#get_raw_mat) pobjmat
  method overlaps (otherpo) =
    let this_coords = sort_coords_rc (self#get_coords ()) in
    let other_coords = sort_coords_rc (otherpo#get_coords ()) in
    let rec intersects coords1 coords2 = 
      match coords1 with
      | [] -> false (* nothing to overlap *)
      | coord1 :: coords1' -> 
		 let filtered = List.filter ~f:(fun x -> x = coord1) coords2 in
                 (match filtered with
                  | [] -> intersects coords1' coords2
                  | _ -> true)
    in
    intersects this_coords other_coords 
      
  method get_coords () = 
    let elems = get_all (self#get_mat) in
    let filtered = List.filter ~f:(fun (_,_,v) -> v = true) elems in
    List.map ~f:(fun (x,y,_) -> (x,y)) filtered

  method clone () =
    let newpo = new pobj pobjmat in
    newpo#set_location location;
    newpo#set_orientation orientation;
    newpo

end

(* example *)
(*
let m1 = fill_matrix (new_matrix (3,3)) true in
let po1 = new pobj m1 in
po1#print
 *)

(* output:
pobj:
TTT
TTT
TTT
- : unit = ()
 *)






(* helper function *)
let create_pobj (w,h) =
  let m0 = new_matrix (w,h) in
  let init_matrix = fill_matrix m0 true in
  new pobj init_matrix

(* e.g., 
let po0 = create_pobj (5,3) in
po0#print;;*)

(* output:
pobj:
TTTTT
TTTTT
TTTTT
- : unit = ()
 *)


let po0 = create_pobj (2,3) in
(*let po1 = create_pobj (2,3) in*)
let po2 = create_pobj (2,3) in
let po3 = create_pobj (2,3) in
let po4 = create_pobj (2,2) in
po2#set_location (0,3);
po3#set_location (1,1);
assert(po0#overlaps po2 = false);
assert(po0#overlaps po3 = true);
assert(po0#is_equal po2 = true);
assert(po0#is_equal po4 = false)



let po0 = create_pobj (3,2) in
let po1 = create_pobj (2,2) in
po0#set_location (0,6);
po1#set_location (0,5);
assert(po0#overlaps po1 = true);
assert(po1#overlaps po0 = true);
po1#set_orientation Right;
assert(po0#overlaps po1 = true);
po0#set_location (0,7);
assert(po0#overlaps po1 = false)

let po0 = create_pobj (10,10) in
let po1 = create_pobj (3,2) in
po0#set_location (5,5);
po1#set_location (7,7);
assert(po0#overlaps po1 = true);
po1#set_location (100,100);
assert(po0#overlaps po1 = false)



let po0 = create_pobj (1,1) in
assert(po0#get_coords() = [(0,0)])

let po1 = create_pobj (2,2) in
assert(po1#get_coords()=[(0,0);(1,0);(0,1);(1,1)])

(* find out the dimensions for a list of coordinates *)

let get_max_coord (coords: (int*int) list) : (int * int ) = 
  let getmax (xmax,ymax) (x,y) = 
    match (xmax < x),(ymax < y) with
    | false, false -> (xmax, ymax)
    | true, false -> (x, ymax)
    | false, true -> (xmax, y)
    | true, true -> (x,y)
  in
  List.fold_left ~f:getmax ~init:(0,0) coords




let coords = [(1,2);(5,3);(0,10);(2,2)] in
(assert ((get_max_coord coords) = (5,10)))



(* comparator for sorting pobj by area defined by max coordinates *)
let compare_pobj po1 po2 = 
  let get_max_coord_area po = 
    let (x,y) =  get_max_coord po in
    x * y
  in
  let po1area = get_max_coord_area (po1#get_coords ()) in
  let po2area = get_max_coord_area (po2#get_coords ()) in
  if po1area = po2area
  then 0
  else
    if po1area > po2area
    then 1
    else -1

(* reversed comparator for decreasing order sort *)
let compare_pobj_inv po1 po2 = (compare_pobj po1 po2) * -1
