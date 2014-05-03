open Core.Std
open BinI

class type bingen = 
object
  method get_next : unit -> bin
end


class bingen_rect ((w,h):(int*int)) : bingen = 
object
  method get_next () = create_bin (w,h)
end


class bingen_rect_hole ((w,h):(int*int)) : bingen = 
object(self)

  val mutable seed_w = 0
  val mutable seed_h = 0

  initializer
    seed_w <- World.rand (w-2);
    seed_h <- World.rand (h-2)

  method private create_custom_bin () : bin =
    let open MatrixI in
    let open BoolMatrix in
    let m0 = new_matrix (w,h) in
    let init_matrix = fill_matrix m0 true in
    let n1 = push_element init_matrix (seed_w,seed_h, false) in
    let n1 = push_element n1 (seed_w + 1,seed_h, false) in
    let n1 = push_element n1 (seed_w,seed_h + 1, false) in
    let n1 = push_element n1 (seed_w + 1,seed_h + 1, false) in
    new bin n1


  method get_next () : bin =
    self#create_custom_bin ()

end
