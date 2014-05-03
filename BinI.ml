open Core.Std
open MatrixI
open PobjI
open MatrixI.BoolMatrix
open WorldObjectBlock
 

class type bin_i = 
object
  (* see if pobj can fit *)
  method can_add : pobj -> bool 
  method draw : unit
  (* actually add pobj to bin *)  
  method push_pobj : pobj -> unit
  (* see if equivalent pobj exists in bin *)
  method has_pobj : pobj -> bool
  (* remove last added pobj *)
  method pop_pobj : unit -> pobj option
  (* gets list of pobj of bin*)
  method get_pobj : unit -> pobj list
  (* gets matrix *)
  method get_mat : BoolMatrix.matrix
  (* gets container coordinates *)
  method get_coords : unit -> (int*int) list
  method set_block_disp : wo_block -> unit
  (* gets all pobj occupied coordinates *)
  method get_occupied_coords : unit -> (int*int) list
  (* gets available/open coordinates *)
  method get_open_coords : unit -> (int*int) list
end


class bin (bin_mat:BoolMatrix.matrix) : bin_i = 
object (self)
  val mutable pobjs : pobj list = []
  val bincolor = 0xdddddd
  val mutable block_disp : wo_block option = None
  val binmat : BoolMatrix.matrix = bin_mat

  method set_block_disp wob = block_disp <- Some wob
  
  method draw = 
    let rec stuff_block_display bd (coords:(int * int) list) color = 
      match coords with
      | [] -> ()
      | (x,y) :: coords' -> bd#push (x,y,color);
          stuff_block_display bd coords' color
    in  
    let rec stuff_pobj_coords bd pos =
      match pos with
      | [] -> ()
      | po :: pos' ->
         let po_coords = po#get_coords () in
         let random_color = Random.int 0xffffff in
         stuff_block_display bd po_coords random_color; 
         stuff_pobj_coords bd pos'
    in  
    match block_disp with
    | None -> () (* no display object set *)
    | Some bd -> 
      (* first display bin *)
      bd#reset ();
      stuff_block_display bd (self#get_coords ()) bincolor;
      stuff_pobj_coords bd pobjs;
      ()
 
  (* checks if all coords for pobj fits in the bin *)
  method private fits_bin coords =
    let rec fb coords =        
      match coords with
      | [] -> true
      | (x,y) :: tl ->
         (* get bin value *)
         let binelv = get_element binmat (x,y)
         in
         (match binelv with
          | false -> false (* outside bin *)
          | _ -> fb tl) (* this is in. keep checking *)
    in
    fb coords

  (* see if pobj can fit *)
  method can_add (po:pobj) = 
    (* check none of po1 els overlaps with pos  *)
    let rec overlap_pobjs po pos =
      match pos with 
      | [] -> false
      | hdpo :: tl -> 
        if po#overlaps hdpo 
        then true
        else overlap_pobjs po tl
    in
    let fb = self#fits_bin (po#get_coords ()) in
    let ol = overlap_pobjs po pobjs in
    match fb, ol with
    | true, false -> true
    | _ -> false

  (* actually add pobj to bin *)  
  method push_pobj (po:pobj) = 
    if self#can_add po then pobjs <- po :: pobjs
    else failwith "illegal pushing of overlapping objects into bin"

  (* see if equivalent pobj exists in bin *)
  method has_pobj (po:pobj) =  
    let rec itr lst = 
      match lst with
      | [] -> false
      | hd :: tl -> if hd#is_equal po 
                   then true 
		   else itr tl
    in
    itr pobjs

  (* remove last added pobj *)
  method pop_pobj () = match pobjs with
    | [] -> None
    | hd :: tl -> 
       pobjs <- tl;
       Some hd
  method get_pobj () = pobjs

  method get_mat = binmat

  method get_coords () = 
    let elems = get_all binmat in
    let filtered = List.filter ~f:(fun (_,_,v) -> v = true) elems in
    List.map ~f:(fun (x,y,_) -> (x,y)) filtered

  method get_occupied_coords () = 
    let rec get_pobj_coords pos acc = 
      match pos with
      | [] -> acc
      | po :: pos' ->
         let po_coords = po#get_coords () in
         get_pobj_coords pos' (po_coords @ acc)
    in
    get_pobj_coords pobjs []
   
  method get_open_coords () = 
    let all_pobj_coords = self#get_occupied_coords () in
    let coords = self#get_coords () in
    (* filter out any bin coordinates that is occupied by pobj *)
    List.filter ~f:(fun (x,y)-> List.for_all 
		~f:(fun poc -> (x,y) <> poc) 
                all_pobj_coords )  
                coords
end


(*helper function *)
let create_bin (w,h) = 
  let m0 = new_matrix (w,h) in
  let init_matrix = fill_matrix m0 true in
  new bin init_matrix


let p0 = create_pobj (2,2) in
let p1 = create_pobj (1,1) in
let p2 = create_pobj (4,4) in
let b = create_bin (3,3) in
assert (b#can_add p0 = true);
assert (b#can_add p1 = true);
assert (b#can_add p2 = false)



let p0 = create_pobj (2,2) in
let p1 = create_pobj (1,1) in
let p2 = create_pobj (2,3) in
let p3 = create_pobj (2,3) in
let b = create_bin (3,2) in
p0#set_location (1,0); (* nudge p0 - it should still fit *)
p1#set_location (3,3); (* set pobj outside bin *)
p3#set_orientation Right; (* rotate p2 so it'll fit *)
assert (b#can_add p0 = true);
assert (b#can_add p1 = false);
assert (b#can_add p2 = false);
assert (b#can_add p3 = true)




let p0 = create_pobj (2,2) in
let p1 = create_pobj (2,2) in
let p2 = create_pobj (2,2) in
let p3 = create_pobj (2,2) in
let p4 = create_pobj (2,2) in
let b = create_bin (4,4) in
p1#set_location (2,0);
p2#set_location (0,2);
p3#set_location (2,2);
p4#set_location (1,1);
assert (b#can_add p4 = true);
assert (b#can_add p0 = true);
b#push_pobj p0;
assert (b#can_add p4 = false);
assert (b#can_add p1 = true);
b#push_pobj p1;
assert (b#can_add p2 = true);
b#push_pobj p2;
assert (b#can_add p3 = true);
b#push_pobj p3;
assert (b#can_add p4 = false)

(* below fails, which shows guard is working *)
(* ; b#push_pobj p4 *)





let p0 = create_pobj (2,2) in
let b = create_bin (3,2) in

assert ( (List.length (p0#get_coords ())) = 4 );
(*let area0 = List.length (b#get_open_coords ()) in
Printf.printf "%d\n" area0;*)
assert ( (List.length (b#get_open_coords ())) = 6) ;
b#push_pobj p0;
let area1 = List.length (b#get_open_coords ()) in
(*Printf.printf "%d\n" area1;*)
assert (area1 = 2)
