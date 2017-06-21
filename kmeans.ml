open Format

type dot = float * float;;
type data = dot list;;
type core = dot list;;

type corenum = Fst | Snd;;
type news = Fst of dot | Snd of dot | Both of dot * dot | Nochenge;;

  
(* create a dot random *)
let mkdots () =
  (Random.float 1.,Random.float 1.);;


(* make n dots list *)
let mk_n_data n =
  let rec dot length =
    if length = n then []
    else (mkdots ()) :: dot (length + 1)
  in dot 0;;

let mkcore () =
  [mkdots ();mkdots ()];;

let dist : dot -> dot -> float = fun d1 d2 ->
  sqrt ((fst d1 -. fst d2) *. (fst d1 -. fst d2) +. (snd d1 -. snd d2) *. (snd d1 -. snd d2)) 
  
let rec neardots : dot -> core -> corenum = fun d c ->
  let c1 = List.nth c 0 in
  let c2 = List.nth c 1 in
  if dist d c2 > dist d c1 then Fst
  else Snd;;

let pushdata : data -> core -> dot list * dot list = fun d c ->
  let rec pushdots =
    fun d (fl,sl) ->
    match d with
    | [] -> (fl,sl)
    | dot :: rest -> 
       (match neardots dot c with
       | Fst -> pushdots rest (dot::fl,sl)
       | Snd -> pushdots rest (fl,dot::sl))
  in
  pushdots d ([],[]) ;;

let middot : dot list -> dot = fun dl ->
  let rec middot' dl' (xt,yt) n =
    match dl' with
    | [] -> (xt /. float_of_int n , yt /. float_of_int n)
    |(x,y)::rest -> middot' rest (xt +. x , yt +. y) (n+1)
  in middot' dl (0.,0.) 0;;
  
let newcore : core -> dot list * dot list -> news = fun c (fl,sl) ->
  match (fl,sl) with
  | ([],_) -> Snd (middot sl)
  | (_,[]) -> Fst (middot fl)
  | _      -> Both (middot fl, middot sl)
;;

let checkmove : core -> core -> bool = fun ncore oldcore ->
  let c1 = List.nth ncore 0 in
  let c2 = List.nth ncore 1 in
  let c3 = List.nth oldcore 0 in
  let c4 = List.nth oldcore 1 in
  if (c1 = c4 && c2 = c3) then false
  else
    not (dist c1 c3 < 0.001 && dist c2 c4 < 0.001);;  
  
let rec kmeans : data -> core -> core * (dot list * dot list) = fun data core ->
  let (fl,sl) = pushdata data core in
  match newcore core (fl,sl) with
  | Fst c  -> ([c;List.nth core 1],(fl,sl))
  | Snd c -> ([List.nth core 0;c],(fl,sl))
  | Both (c1,c2) -> if checkmove [c1;c2] core then kmeans data [c1;c2] 
  else ([c1;c2],(fl,sl));;

(*let c1 = mkcore ();;
let c2 = mkcore ();;

let nc1 = [(1,1);(5,5)];;
let nc2 = [(1,2);(5,6)];;

let x = mk_n_data 10;;
let y = mk_n_data 4;;
  
checkmove nc1 nc2;;
  
newcore c1 (x,y);; 

let (fl,sl) = pushdata x c1;;
  newcore c1 (fl,sl);;
    
checkmove [(62,37);(34,73)] c1;;

  kmeans x c1;;  
#trace kmeans;;  

  kmeans (mk_n_data 20) (mk_n_data 2);;
 *)
let _ =
  let n = int_of_string (read_line ()) in
  let (cores,(data1,data2)) = kmeans (mk_n_data n) (mk_n_data 2) in
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
  data_output data2;close_out oc 
;;
