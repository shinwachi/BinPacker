open Core.Std


(** Enum of directions *)
type direction = | Up | Down | Left | Right


(** List of all directions **)
let alldir : direction list = [Up; Down; Left; Right]

let ord (n:int) : direction =
  match n with 
  | 0 -> Up | 1 -> Down | 2 -> Left | 3 -> Right | _ -> Up


  
