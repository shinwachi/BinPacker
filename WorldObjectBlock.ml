open Core.Std
open WorldObjectI

(** An abstract implementation of world_object_i that provides some helper
    functionality.
 *)

class world_object_block (pos: int*int) (color:int) : world_object_i =
object (self)

  val mutable pos : int*int = pos
  val mutable width : int = World.obj_width
  val mutable height : int = World.obj_height

  initializer
    World.add pos (self :> world_object_i) ;

  method private draw_rectangle =
  Graphics.set_color color ;
  Graphics.fill_rect ((fst pos) * width)  ((snd pos) * height) width height
              
  method get_name = "block"
  method get_pos = pos
  method get_width = width
  method get_height = height
  method draw = self#draw_rectangle
  method draw_z_axis = 3

end

class type wo_block_i =
object
  (** Insert the co-ordiante & color in world object. *)
  method push :  (int * int * int) -> unit

  (** Reset all the element link to this block in the World. *)
  method reset : unit -> unit

  (** How to draw the object. *)
  method draw : unit

end

class wo_block (coord: int*int) : wo_block_i =
object (self)

  val mutable blocklist : (int * int * int) list = []
  val mutable co_ordinate : (int * int) = coord

  method draw =  self#draw_block_rectangle ()

  method private draw_block_rectangle () =
    let rec aux bk =
      match bk with
      | [] -> ()
      | (x, y, col) :: tl -> ignore(new world_object_block (x,y) col); aux tl
    in
    aux blocklist
			     
  method push (a :int * int * int) =
    let (x,y,c) = a in
    blocklist <- (x + (fst co_ordinate), y + (snd co_ordinate), c)
		 :: blocklist; ()
    
  method reset () =
    (* removing object world event. *)
    let rec aux bk =
      match bk with
      | [] -> ()
      | (x, y, _) :: tl -> World.remove_xy (x,y); aux tl
    in
    aux blocklist;
    (* reset local variable. *)
    blocklist <- [];
    ()

end
