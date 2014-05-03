open Core.Std

let packslist : string list ref = ref []
let bin_average_list : int list ref = ref []

class type bin_avg_i = 
object
  method calc_pack : BinI.bin list -> WorldBins.algotype -> unit
end
  
class bin_avg : bin_avg_i =
object (self)


  val mutable bin_block_count : int = 0
  val mutable pobj_block_count : int = 0
  val mutable bin_count : int = 0
  val mutable pobj_count : int = 0

  method private bin_block_counter (b:BinI.bin) : int =
    let y = b#get_mat in
    let x = MatrixI.BoolMatrix.get_all y in
    let z =  List.length x in
    bin_block_count <- bin_block_count + z;
    z
    
  method private pobj_block_counter (p:PobjI.pobj) : int =
    let y = p#get_mat in
    let xs = MatrixI.BoolMatrix.get_all y in
    let y2 = 0 in
    let rec aux xl (y1:int) : int =
      match xl with
      | [] -> y1
      | (_, _, true) :: xs' ->
	 pobj_block_count <- pobj_block_count + 1;
	 aux xs' (y1 + 1)
      | _ -> y1
    in
    aux xs y2

  method private pobjs_block (bn:BinI.bin) : int =
    let y = 0 in
    let rec aux (xs: PobjI.pobj list)  (y1:int) : int =
      match xs with
      | [] -> y1
      | x :: xs' ->
	 pobj_count <- pobj_count + 1;
	 let y2 = y1 + self#pobj_block_counter x in
	 aux xs' y2
    in
    aux (bn#get_pobj ()) y 

  method private update_pack (bt:WorldBins.algotype) : unit =
    let eff = (pobj_block_count * 100) / bin_block_count in
    let str : string = Int.to_string eff ^ 
			 "% Bins:" ^ Int.to_string bin_count ^
			   " Pobj:" ^ Int.to_string pobj_count ^
			     " Bin_units:" ^  Int.to_string bin_block_count ^
			       " Pobj_units:" ^  Int.to_string pobj_block_count ^
				 " Bin_Median:" ^ 
				   Int.to_string (
					      match (List.nth 
						       (List.sort 
							  ~cmp:(fun x y -> Int.compare x y) 
							  !bin_average_list) 
						       ((List.length !bin_average_list)/2)) with
					      | None -> -1
					      | Some x -> x) 
						 
    in
    let build_text : string =
      match bt with
      | FFD -> 
	 "FFD:" ^ str
      | FF ->
	 "FF:" ^ str
      | BF ->
	 "BF:" ^ str
      | Nothing ->
	 "Plot:" ^ str
    in
    let timetook : float =
      match (List.find !WorldBins.timecapture ~f:(fun x -> fst x = bt)) with
      | None -> -99999.99999
      | Some x -> snd x
    in
    let open Out_channel in
    let file = "BinPackingReport.dat" in
    let oc = create file ~append:true in
    begin
      if not (List.exists !packslist ~f:(fun x -> x = build_text))
      then(
	fprintf oc "%s\n" (build_text ^ " TimeTook:" ^ Float.to_string timetook);
	!packslist <- build_text :: !packslist)
    end;
    close oc;

    let wob = 
      new WorldObjectText.world_object_text build_text (6,65) Graphics.black in
    wob#draw
	  
  method calc_pack (blst: BinI.bin list) (pack: WorldBins.algotype) : unit = 
    bin_block_count <- 0;
    pobj_block_count <- 0;
    bin_count <- 0;
    pobj_count <- 0;
    (*packslist := [];*)
    bin_average_list := [];
    let rec aux(xs : BinI.bin list): unit =
      match xs with
      | [] -> ()
      | x :: xs'  -> 
	 bin_count <- bin_count + 1;
	 let b = self#bin_block_counter x in
	 let p = self#pobjs_block x in
	 let z = (p * 100) / b in
	 !bin_average_list <- z :: !bin_average_list;
	 aux xs'
    in
    aux blst;
    self#update_pack pack

end
