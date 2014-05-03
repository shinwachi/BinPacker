open Core.Std
open PobjI
open MatrixI.BoolMatrix

(* interface for generation of pObjs *)
class type generator = 
object
  method get_next : unit -> pobj option
  method get_remaining : unit -> pobj list
end


class generator_pobj_lst (pobj_lst:pobj list) : generator = 
object
  val mutable pobjs = pobj_lst
  method get_next () = 
    match pobjs with
    | [] -> None
    | hd :: tl ->
      pobjs <- tl; Some (hd#clone())
  method get_remaining () = pobjs
end


(* test *)
let p0 = create_pobj (1,1) in
let p1 = create_pobj (1,1) in
let g = new generator_pobj_lst [p0;p1] in
let p0' = g#get_next () in
let _ = g#get_next () in
let p2 = g#get_next () in
match p0' with
| None -> failwith "failed test"
| Some po0 -> 
   assert (p2 = None);
   assert ((get_all po0#get_mat) = [(0,0,true)])



class  generator_rect (count:int) ((w,h):(int * int)) : generator = 
object  (self)
  val mutable counter = count
  method get_next () = 
    if (counter > 0)
    then (counter <- (counter-1);
          Some (create_pobj (w,h)))
    else None
  method get_remaining = 
    let rec getallpo () = 
      match self#get_next () with
      | None -> []
      | Some po -> po :: getallpo ()
    in
    getallpo
end



(* test *)
let g = new generator_rect 2 (1,1) in
let p0 = g#get_next () in
let _ = g#get_next () in
let p2 = g#get_next () in
match p0 with
| None -> failwith "failed test"
| Some po0 -> 
   assert (p2 = None);
   assert ((get_all po0#get_mat) = [(0,0,true)])




class generator_random (count:int) ((w,h):(int * int)) : generator = 
object (self)
  val mutable counter = count
  method get_next () = 
    if (counter > 0)
    then (counter <- (counter-1);
	  let wb = World.rand (w + 1) in
	  let hb = World.rand (h + 1) in
          Some (create_pobj (wb,hb)))
    else None
  method get_remaining =
    let rec getallpo ()=
      match self#get_next () with
      | None -> []
      | Some po -> po :: getallpo ()
    in
    getallpo
end







