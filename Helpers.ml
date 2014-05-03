open Core.Std
(** A package of generic and useful helper functions. *)
exception TODO

(*************************)
(***** Array Helpers *****)
(*************************)

(** Modify each element of an array in place *)
let array_map_modify (f:'a -> 'a) (arr:'a array) : unit =
  Array.iteri ~f:(fun i _ -> arr.(i) <- f (arr.(i))) arr

(************************)
(***** List Helpers *****)
(************************)

(** The list containing n replicas of e *)
let rec replicate (n:int) (e:'a) : 'a list =
  if n = 0 then [] else e::replicate (n-1) e

(** The cross produce of lists xs and ys.
    result = \[ (x,y) | x in xs and y in ys \] *)
let rec cross (xs:'a list) (ys:'b list) : ('a*'b) list =
  match xs with
  | [] -> []
  | hd::tl -> List.map ~f:(fun y -> (hd,y)) ys @ cross tl ys

(** The monotonically increasing list containing each number in the range
    between n1 and n2 (inclusive) *)
let rec range (n1:int) (n2:int) : int list =
  if n1 > n2 then [] else n1::range (n1+1) n2

(** The list of unique elements in xs. *)
let rec unique (xs : 'a list) : 'a list =
  match xs with
  | [] -> []
  | hd::tl ->
      let tl' = unique tl in
      if List.mem tl' hd then tl' else hd::tl'

(** The bool result on list of all true elements in xs. *)
let rec alltrue (xs : 'a list) : bool =
  match xs with
  | [] -> true
  | (_, _, true) :: tl -> alltrue tl
  | _ -> false

(** Find maximum no. of rows and column from matrix in ms. *)
let no_row_column (ms:(int * int * bool) list) : int * int =
  let rec no_row_column_rec m row col =
    match m with
    | [] -> (row + 1,col + 1)
    | (r, c, _) :: tl -> 
       no_row_column_rec tl
	 (if r > row then r else row) 
	 (if c > col then c else col)
  in
  no_row_column_rec ms 0 0

(**************************)
(***** Number Helpers *****)
(**************************)

(** Bound x between low and high. *)
let bound (low:int) (high:int) (x:int) : int = min (max low x) high

(********************************)
(***** Random Value Helpers *****)
(********************************)

(** call f with probability (1/p) and g if f is not called *)
let with_inv_probability_or (r:int->int) (p:int)
                            (f:unit->'a) (g:unit->'a) : 'a =
  if r p = 0 then f () else g ()

(** Call f with probability (1/p) (using r to generate random numbers) *)
let with_inv_probability (r:int->int) (p:int) (f:unit->unit) : unit =
  with_inv_probability_or r p f (fun () -> ())

(** Call one of the functions in the list with equal probability. *)
let with_equal_probability (r:int->int) (fs:(unit -> unit) list) : unit =
  (List.nth_exn fs (r (List.length fs))) ()

(******************************)
(******Matrix Helpers**********)
(******************************)
(* cribbed from Chris Conway (2008) http://stackoverflow.com/
 * questions/243864/
 * what-is-the-ocaml-idiom-equivalent-to-pythons-range-function *)

let (--) i j =
  let rec aux n acc = 
    if n < i then acc else aux (n-1) (n::acc)
  in aux j [];;

assert (1--2 = [1;2])
assert (1--5 = [1;2;3;4;5])
assert (5--10 = [5;6;7;8;9;10])

let rec zip l1 l2 =
  match l1,l2 with
  | [], _ | _,[] -> []
  | (h1::t1),(h2::t2) -> (h1,h2) :: (zip t1 t2)

(*****************************)
(*******Tuple Min Max*********)
(*****************************)
let deoption x =
  match x with
  | None -> failwith "Invalid"
  | Some a -> a

let acc_x l =
  fst (deoption (List.nth l 0))

let acc_y l =
  snd (deoption (List.nth l 0))

let cood_x lst f =
  List.fold_left ~init:(acc_x lst) ~f:(fun acc x -> f acc (fst x)) lst

let cood_y lst f =
  List.fold_left ~init:(acc_y lst) ~f:(fun acc y -> f acc (snd y)) lst

let sort_on_y lst = 
  List.sort ~cmp:(fun (_,x,_) (_,y,_) -> Int.compare x y) lst


(* sort coordinates, first by row, then by column *)
let sort_coords_rc (coords: (int*int) list) =
  let cmp (x1,y1) (x2,y2) = 
    if y1 < y2 then -1
    else 
    if y1 > y2 then 1
    else
    if x1 < x2 then -1
    else
    if x1 > x2 then 1
    else 0
  in
  List.sort ~cmp:cmp coords
         
