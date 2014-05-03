open Core.Std

open Helpers

open BinI
open PobjI
 
open GeneratorI
open BinGenI

open Direction


(* interface for the packing algorithms *)
class type packer = 
object
  (* generators *)
  val mutable bingen  : bingen (* stream of bins *)
  val mutable pobjgen : generator (* stream of pobjs *)
  val mutable bins    : bin list (* filled bins *)
  method get_bins  : unit -> bin list
  (* putting algorithm to work *)
  method pack_bins : unit -> unit
  method get_name  : unit -> WorldBins.algotype

end


(* this will use one bin per pobj in list *)
class dumb_packer (bingen_init:bingen) (gen_init:generator) :packer = 
object
  val mutable bingen  = bingen_init
  val mutable pobjgen = gen_init
  val mutable bins    = [] 
  
  method get_bins () = bins

  method pack_bins () =
    let rec pkbn () =   
    match pobjgen#get_next () with
    | None   -> ()
    | Some p ->
       let b = bingen#get_next () in
       if b#can_add p = true
       then 
         (b#push_pobj p;
          bins <- b::bins;
          pkbn ())
       else
         failwith "can't fit pobj into bin"; ()
    in
    pkbn ()

  method get_name () : WorldBins.algotype =
    Nothing
end

(* test *)
let bg = new bingen_rect (3,3) in
let pg = new generator_rect 5 (1,1) in
let pkr = new dumb_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins () in
assert ((List.length bs) = 5);



(* first fit packer *)
class ff_packer (bingen_init:bingen) (gen_init:generator) :packer =
object (self)
  val mutable bingen  = bingen_init
  val mutable pobjgen = gen_init
  val mutable bins    = [] 
  
  method get_bins () = bins

  (* checks all orientation and location *)
  method private fitIntoBin (bn:bin) po : (int * int * direction) option = 
    let bincoords = bn#get_coords () in
    (* rotates pobj until it fits. returns None if nothing fits *)
    let rec check_dirs dirs = 
      match dirs with
      | [] -> None
      | d :: dirs' ->
        (po#set_orientation d); 
        if (bn#can_add po)
        then (Some d)
        else (check_dirs dirs')
    in
    (* goes through all bin coordinates to see if it fits *)
    let rec check_pos coords =
      match coords with
      | [] -> None
      | (x,y) :: coords' ->
        po#set_location (x,y); 
        (match (check_dirs alldir) with
         | Some dir -> Some (x,y,dir)
         | None -> check_pos coords')
    in
    check_pos (sort_coords_rc bincoords)

  
  method pack_bins () =
    (* iterate through bins and try to fit pobj *)
    let rec pkbn po bns = 
      match bns with
      | [] -> false (* no bins fit pobj *)
      | bn :: bns' ->
        (match (self#fitIntoBin bn po) with
         | Some (x,y,d) ->
           po#set_location (x,y);
           po#set_orientation d;
           bn#push_pobj po; true
         | None -> pkbn po bns')
    in
    (* main routine *) 
    let rec main () =
      (* iterate through each pobj *)
      match pobjgen#get_next () with
      | None -> ()
      | Some p -> 
        (* first check existing bins can contain pobj *)
        (if (pkbn p bins) = false
        then
          (* new bin should always fit *)
          let bn = bingen#get_next () in
          (match (self#fitIntoBin bn p) with
           | Some (x,y,d) ->
             p#set_location (x,y);
             p#set_orientation d;
             bn#push_pobj p;
             bins <- bn::bins
           | None -> failwith "pobj bigger than bin")
(*
          if (false) (*(pkbn p [b]) = false *)
          then failwith "can't fit pobj into new bin"
          else (bins <-  b::bins; ()) (* add new bin *) *)
        else
          ());
        main (); (* keep going *)
    in
    main ()

  method get_name () : WorldBins.algotype =
    FF
end



let bg = new bingen_rect (3,3) in
let pg = new generator_rect 5 (1,1) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 1);

let bg = new bingen_rect (2,2) in
let pg = new generator_rect 5 (1,1) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 2);

let bg = new bingen_rect (2,2) in
let pg = new generator_rect 5 (2,2) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 5);

let bg = new bingen_rect (4,4) in
let pg = new generator_rect 5 (3,3) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 5);

let bg = new bingen_rect (3,1) in
let pg = new generator_rect 5 (1,3) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 5);

let bg = new bingen_rect (3,2) in
let pg = new generator_rect 5 (1,3) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 3);

let bg = new bingen_rect (6,2) in
let pg = new generator_rect 5 (1,3) in
let pkr = new ff_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 2);


(* first fit decreasing *)
(* requires comparator *)
(* first, figure out area needed to fit the pobj (assume no blank space?) *)
class ffd_packer (bingen_init:bingen) (gen_init:generator) : packer =
object
  inherit ff_packer bingen_init gen_init

  initializer
  let rec getallpo g = 
    match pobjgen#get_next () with
    | None -> []
    | Some po -> po :: getallpo g
  in
  let pos = getallpo pobjgen in
  let pos_sort = List.sort ~cmp:compare_pobj_inv pos in
  pobjgen <- new generator_pobj_lst pos_sort

  method! get_name () =
    FFD
end


let bg = new bingen_rect (6,2) in
let pg = new generator_rect 5 (1,3) in
let pkr = new ffd_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 2);







(* best fit *)
(* requires capacity calculation after an object has been added *)
class bf_packer (bingen_init:bingen) (gen_init:generator) : packer =
object (self)
  inherit ff_packer bingen_init gen_init

  val mutable pobjs = []
  
  initializer
  let rec getallpo g = 
    match pobjgen#get_next () with
    | None -> []
    | Some po -> po :: getallpo g
  in
  let pos = getallpo pobjgen in
  let pos_sort = List.sort ~cmp:compare_pobj pos in
  pobjs <- pos;
  pobjgen <- new generator_pobj_lst pos_sort


  (* checks all orientation and location *)
  (* TODO: sort the coordinates (don't assume order like now) *)
  method private fitIntoBin (bn:bin) po : (int * int * direction) option = 
    let bincoords = bn#get_coords () in
    let rec check_dirs dirs = 
      match dirs with
      | [] -> None
      | d :: dirs' ->
        (po#set_orientation d); 
        if (bn#can_add po)
        then (Some d)
        else (check_dirs dirs')
    in
    let rec check_pos coords =
      match coords with
      | [] -> None
      | (x,y) :: coords' ->
        po#set_location (x,y); 
        (match (check_dirs alldir) with
         | Some dir -> Some (x,y,dir)
         | None -> check_pos coords')
    in
    check_pos bincoords

  
  method private calc_remaining_capacity bn po : int option = 
    match self#fitIntoBin bn po with
    | None -> None
    | Some _ ->
       (* get pobject area *)
       let po_sp = List.length (po#get_coords ()) in
       (* get bin open area *)
       let bn_sp = List.length (bn#get_open_coords ()) in
       Some (bn_sp - po_sp) (* remaining capacity *)


  (* inner recursion for trying out all pobjs *)
  method private fit_pobjs_in_bin bin pobj_list acc = 
    let rec fitpos bn pos (acc:(bin*pobj) option)=
      match pos, acc with
      | po :: pos', None ->
         let remcap = (self#calc_remaining_capacity bn po) in
         (match remcap with
          | None -> fitpos bn pos' None
          | Some _ -> fitpos bn pos' (Some (bn, po)))
      | po :: pos', Some (acc_bn, acc_po) ->
        (* calculate the current bn+po pair's remaining capacity *)
        let remcap = (self#calc_remaining_capacity bn po) in
        (* calculate the "accumulated" bn+po pair's remaining capacity *)
        let acc_remcap = (self#calc_remaining_capacity acc_bn acc_po) in
        (match remcap, acc_remcap with
         | None, None 
         | None, _   -> fitpos bn pos' acc
         | _, None   -> fitpos bn pos' (Some (bn, po))
         | Some remcapi, Some acc_remcapi ->
           (* this part is where minimum capacity comes from *) 
           if remcapi < acc_remcapi
           then fitpos bn pos' (Some (bn, po)) (* new winner *)
           else fitpos bn pos' acc)
      | _ -> acc
    in
    fitpos bin pobj_list acc


  method private retrieve_min_capacity_pair bns pos : (bin*pobj) option = 
    (* outer recursion for trying all bins *)
    let rec fitbins bns pos (acc:(bin*pobj) option) =
      match bns with
      | [] -> acc
      | bn :: bns' -> 
         let acc' = self#fit_pobjs_in_bin bn pos acc in
         fitbins bns' pos acc'
    in
    (* compare all bin x pobj capacities and get minimum capacity *)
    fitbins bns pos None
         

  method! pack_bins () = 
    (* obtain the "best fit" bin x pobj pair 
     * (minimum capacity remaining after packing) *)
    let rec pbfunc () = 
      
      match (self#retrieve_min_capacity_pair bins pobjs) with
      (* no match *)
      | None ->
          (match pobjs with
           | [] -> () (* no pobjs left.  done. *)
           | _ -> 
              let newbin = bingen#get_next () in
              bins <- newbin :: bins;
               pbfunc ()) (* more pobjs - keep packing *)
      (* match in existing bin *)
      | Some (bn, po) ->
          (match (self#fitIntoBin bn po) with
           | Some (x,y,d) ->
               po#set_location (x,y);
               po#set_orientation d;
               bn#push_pobj po;
               (* since po is in a bin, remove from pobjs *)
               pobjs <- List.filter 
                          ~f:(fun apo -> not (phys_equal po apo)) 
                          pobjs;
               pbfunc ()
           | _ -> failwith "failed to fit po - this should never happen") 
    in
    pbfunc ();
    
  method! get_name () : WorldBins.algotype =
    BF
end


(* test *)
let bg = new bingen_rect (6,2) in
let pg = new generator_rect 5 (1,3) in
let pkr = new bf_packer bg pg in
pkr#pack_bins ();
let bs = pkr#get_bins ()
in 
assert ((List.length bs) = 2);
 

 
