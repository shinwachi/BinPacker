open Core.Std
open WorldObjectI

(** An abstract implementation of world_object_i that provides some helper
    functionality.
 *)

class world_object_text (txt: string) (pos: int*int) fg : world_object_i =
object (self)

  val mutable pos : int*int = pos
  val mutable width : int = World.obj_width
  val mutable height : int = World.obj_height

  initializer
    World.add pos (self :> world_object_i) ;

  method private draw_text =
    Graphics.moveto ((fst pos)*width+2) ((snd pos)*height);
    Graphics.set_color fg ;
    Graphics.draw_string txt
              
  method get_name = "text"
  method get_pos = pos
  method get_width = width
  method get_height = height
  method draw = self#draw_text
  method draw_z_axis = 4

end
