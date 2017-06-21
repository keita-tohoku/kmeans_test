open Format

type dot = float * float;;
type data = dot list;;
type core = dot list;;

type corenum = Fst | Snd | Trd;;
type newcores = All of core
	      | One of int * dot
	      | Two of int * int * core;;
   
(* create a dot random *)
let mkdots () =
  (Random.float 1.,Random.float 1.);;


(* make n dots list *)
let mk_n_data n =
  let rec dot length =
    if length = n then []
    else (mkdots ()) :: dot (length + 1)
  in dot 0;;

(* make 2 cores (if you need more cores, you can use mk_n_data as cores.) *)
let mkcore () =
  [mkdots ();mkdots ()];;

  
let dist : dot -> dot -> float = fun d1 d2 ->
  sqrt ((fst d1 -. fst d2) *. (fst d1 -. fst d2) +. (snd d1 -. snd d2) *. (snd d1 -. snd d2)) 
  
let rec neardots : dot -> core -> corenum = fun d c ->
  let dist1 = dist d (List.nth c 0) in
  let dist2 = dist d (List.nth c 1) in
  let dist3 = dist d (List.nth c 2) in
  if dist2 > dist1 && dist3 > dist1 then Fst
  else if dist1 > dist2 && dist3 > dist2 then Snd
  else Trd;;

let pushdata : data -> core -> dot list * dot list * dot list = fun d c ->
  let rec pushdots =
    fun d (fl,sl,tl) ->
    match d with
    | [] -> (fl,sl,tl)
    | dot :: rest -> 
       (match neardots dot c with
       | Fst -> pushdots rest (dot::fl,sl,tl)
       | Snd -> pushdots rest (fl,dot::sl,tl)
       | Trd -> pushdots rest (fl,sl,dot::tl))
  in
  pushdots d ([],[],[]) ;;

let middot : dot list -> dot = fun dl ->
  let rec middot' dl' (xt,yt) n =
    match dl' with
    | [] -> (xt /. float_of_int n , yt /. float_of_int n)
    |(x,y)::rest -> middot' rest (xt +. x , yt +. y) (n+1)
  in middot' dl (0.,0.) 0;;
  
let newcore : core -> dot list * dot list * dot list -> newcores = fun c (fl,sl,tl) ->
  match (fl,sl,tl) with
  | ([],[],_) -> One (3,middot tl)
  | ([],_,[]) -> One (2,middot sl)
  | (_,[],[]) -> One (1,middot fl)
  | ([],_,_)  -> Two (2,3,[middot sl;middot tl])
  | (_,[],_)  -> Two (1,3,[middot fl;middot tl])
  | (_,_,[])  -> Two (1,2,[middot fl;middot sl])
  | _         -> All [middot fl; middot sl;middot tl]
;;

let checkmove : core -> core -> bool = fun ncore oldcore ->
  let c1 = List.nth ncore 0 in
  let c2 = List.nth ncore 1 in
  let c3 = List.nth oldcore 0 in
  let c4 = List.nth oldcore 1 in
  if (c1 = c4 && c2 = c3) then false
  else
    not (dist c1 c3 < 0.001 && dist c2 c4 < 0.001);;  
  
let rec kmeans : data -> core -> core * dot list * dot list * dot list = fun data core ->
  let (fl,sl,tl) = pushdata data core in
  match newcore core (fl,sl,tl) with
  | One (x,c)  -> begin match x with
			| 1 -> ([c;List.nth core 1;List.nth core 2],fl,sl,tl)
			| 2 -> ([List.nth core 0;c;List.nth core 2],fl,sl,tl)
			| 3 -> ([List.nth core 0;List.nth core 1;c],fl,sl,tl) end
  | Two (x,y,cl) -> begin match (x,y) with
			  | (1,2) -> if checkmove cl [List.nth core 0;List.nth core 1]
				     then kmeans data (cl@[List.nth core 2])
				     else ((cl@[List.nth core 2]),fl,sl,tl)
			  | (1,3) -> if checkmove cl [List.nth core 0;List.nth core 2]
				     then kmeans data [List.nth cl 0;List.nth core 1;List.nth cl 1]
				     else ([List.nth cl 0;List.nth core 1;List.nth cl 1],fl,sl,tl)
			  | (2,3) ->  if checkmove cl (List.tl core)
				      then kmeans data ((List.hd core) :: cl)
				      else (((List.hd core) :: cl),fl,sl,tl) end
  | All cl -> if checkmove (List.tl cl) (List.tl core)
	      then kmeans data cl
	      else if checkmove [List.nth cl 0;List.nth cl 1] [List.nth core 0;List.nth core 1]
	      then kmeans data cl
	      else (cl,fl,sl,tl)
;;

let _ =
  let n = int_of_string (read_line ()) in
  let (cores,data1,data2,data3) = kmeans (mk_n_data n) (mk_n_data 3) in
  let file = "kmeans.txt" in
  let oc = open_out file in
  let ppf = formatter_of_out_channel oc in
  let rec core_output core =
    match core with
    | [] -> fprintf ppf "\n"
    | c :: rest -> fprintf ppf "%.4f %.4f\n" (fst c) (snd c);core_output rest
  in 
  let rec data_output data =
    match data with
    | [] -> fprintf ppf "\n"
    | d :: rest -> fprintf ppf "%.4f %.4f\n" (fst d) (snd d);data_output rest
  in
  core_output cores;fprintf ppf "the data belong core1 is \n";
  data_output data1;fprintf ppf "the data belong core2 is \n";
  data_output data2;fprintf ppf "the data belong core3 is \n";
  data_output data3;close_out oc 
;;
