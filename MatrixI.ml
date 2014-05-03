open Core.Std
open Direction
open Helpers

(** An Interface for Matrix. **)

module type MATRIX =
sig  
  (** the matrix type **)
  type elt
  type matrix

  (** width, height as argument **)
  val new_matrix  : (int * int) -> matrix
  val push_element : matrix -> (int * int * elt) -> matrix
  val get_width : matrix -> int
  val get_height : matrix -> int
  (* gets element at x,y coordinate *)
  val get_element : matrix -> (int * int) -> elt
  (* get all coordinates and its elements *)
  val get_all : matrix -> (int * int * elt) list
  (* transform matrix in directions (Up is 0 deg) *)
  val rotate : matrix -> direction -> matrix
  (* returns translated matrix *)
  val translate : matrix -> (int * int) -> matrix
  (* whether the two matrices have equivalent values *)
  val is_equal : matrix -> matrix -> bool 
end


module BoolMatrix : MATRIX with type elt = bool =
struct
  type elt = bool
  type matrix = int * int * (int * int * elt) list
  let new_matrix (width, height) = (width, height, [])
  let get_width matrix = match matrix with
    | (w, _, _) -> w
  let get_height matrix = match matrix with
    | (_, h, _) -> h
  let get_element matrix (x,y) =
    let rec get (x,y) l = 
      (match l with
       | [] -> false
       | (xv,yv,v) :: _  when (( xv = x ) && ( yv = y)) -> v
       | _ :: tl -> get (x,y) tl)
    in
    match matrix with
    | (_, _, l) -> get (x,y) l

  let get_all matrix = match matrix with
    | (_, _, l) -> l

  let push_element matrix (x,y,e) = 
    let rec push (x,y,e) l = 
      (match l with
       | [] -> [(x,y,e)]
       | (xv,yv,_) :: tl when ((xv = x) && (yv = y)) -> (x,y,e) :: tl
       | hd :: tl -> hd :: (push (x,y,e) tl))
    in
    match matrix with
    | (w, h, l) -> (w,h, (push (x,y,e) l))
 
  let rotate (w, h, l) direction =
    let rot_elem_90cw (x,y,e) = ( h-1-y, x, e) in
    let rotmat (w, h, l) =
      (h, w, (List.map ~f:(fun lel -> rot_elem_90cw lel) l))
    in
    match direction with
    | Up -> (w, h, l)
    | Right -> rotmat ( w,h,l)
    | Down -> rotmat ( rotmat (w, h, l))
    | Left -> rotmat ( rotmat ( rotmat (w, h, l)))
     
  let translate (w, h, l) (x, y) = 
    let transel (x, y) (ex, ey, e) = (ex+x, ey+y, e) in
    (w, h, (List.map ~f:(fun lel -> transel (x,y) lel) l))

  let is_equal m1 m2 =
    let rec cmp l1 mm2 = 
      match l1 with
      | [] -> true
      | (x,y,v1) :: l1' ->
         let v2 = get_element mm2 (x,y) in
         if v1 <> v2 
         then false
         else cmp l1' mm2
   in 
   let (_,_,l1) = m1 in
   let (_,_,l2) = m2 in
   (cmp l1 m2) && (cmp l2 m1) (* not terribly efficient *)
    
end

open BoolMatrix

(* helper function *)
let push_row mat y rowvals = 
  let colidxs = 0--(List.length rowvals - 1) in
  let pairlst = zip colidxs rowvals in
  let rec psh m pl  = 
    match pl with
    | [] -> m
    | (x,v) :: tl ->
       let m' = push_element m (x,y,v) in
       psh m' tl
  in
  psh mat pairlst


;;

let m1 = new_matrix (3,4) in
let m2 = push_row m1 0 [true;false;true] in
let m3 = push_row m2 3 [false;true;false] in
assert (get_element m2 (0,0) = true);
assert (get_element m2 (1,0) = false);
assert (get_element m2 (2,0) = true);
assert (get_element m3 (0,3) = false);
assert (get_element m3 (1,3) = true);
assert (get_element m3 (2,3) = false)


;;

(* fills all mat coords with v *)
let fill_matrix mat v =
  let colidxs = 0--((get_width mat) - 1) in
  let rowidxs = 0--((get_height mat) - 1) in
  let m = ref mat in
  List.iter rowidxs ~f:(fun y ->
    List.iter colidxs ~f:(fun x ->
      (* Printf.printf "%d,%d\n" x y; *)
      m := push_element !m (x,y,v)
    )
  );
  !m
;;

let m1 = fill_matrix (new_matrix (2,2)) true in
assert (get_all m1 = [(0,0,true);(1,0,true);(0,1,true);(1,1,true)])


;;
(* prints all mat coords *)
let print_matrix mat =
  let colidxs = 0--((get_width mat) - 1) in
  let rowidxs = 0--((get_height mat) - 1) in
  List.iter rowidxs ~f:(fun y ->
    List.iter colidxs ~f:(fun x ->
      if (get_element mat (x,y)) = true
      then print_string "T"
      else print_string "F"
    );
    print_string "\n"
  )
;;



(*
let m1 = fill_matrix (new_matrix(4,3)) true in
let m2 = push_element m1 (1,0, false) in
print_matrix m1;
print_matrix m2
 *)

(* output:
TTTT
TTTT
TTTT
TFTT
TTTT
TTTT
- : unit = ()
 *)


open BoolMatrix
let m = new_matrix (3,2) in
let n = push_element m (0,1, true) in 
assert(get_width m = 3);
assert(get_height m = 2);
assert(get_element n (0,1) = true);
assert(get_element n (1,1) = false)

(* 3/4 Matrix --> 4/3 Matrix
              1 1 1
1 0 0 1       1 0 0
1 0 1 0  -->  0 1 0
1 1 0 1       1 0 1

*)

let m1 = new_matrix (3,4) in
let n1 = push_element m1 (0,0, true) in
let n1 = push_element n1 (0,1, true) in
let n1 = push_element n1 (0,3, true) in

let n1 = push_element n1 (1,0, true) in
let n1 = push_element n1 (1,2, true) in

let n1 = push_element n1 (2,0, true) in
let n1 = push_element n1 (2,3, true) in

let m2 = rotate n1 Right in
assert(get_width m2 = 4);
assert(get_height m2 = 3);
assert(get_element m2 (0,0) = true);
assert(get_element m2 (1,0) = false)


