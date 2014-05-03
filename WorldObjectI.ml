open Core.Std
open Event51

(** world_object_i is the super type of all objects in the world. Note that
    world_object_i is defined with "class type" making it an interface. *)
class type world_object_i =
object
  (** The name of the object. get_name is used for displaying purposes. *)
  method get_name : string

  (** The position of the object.  This is the primary way to tell if one
      object is on the same square as another *)
  method get_pos : int*int

  (** The width of the object. *)
  method get_width : int

  (** The height of the object. *)
  method get_height : int

  (** How to draw the object. *)
  method draw : unit

  (** The z-axis used for drawing the object. Objects with higher z-axis will be
   * drawn on top of objects with lower z-axis. *)
  method draw_z_axis : int

end
