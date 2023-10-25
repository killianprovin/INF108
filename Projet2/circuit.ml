let nb_bits = 16 
type tension = int
let meme_tension = ref []
let portes_nand = ref []
let portes_delai = ref []

let nb_tensions = ref 0

let nouvelle_tension =
  fun () -> incr nb_tensions ; (!nb_tensions-1)
  
let zero = nouvelle_tension ()
let un = nouvelle_tension ()

(* let get_ram () = *)
(*   let adr_lue_1 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let adr_lue_2 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let adr_ecrite = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let v_ecrite = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let v_lue_1 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let v_lue_2 = Array.init nb_bits (fun () -> nouvelle_tension()) in *)
(*   let ecrit = nouvelle_tension () in *)
(*   (adr_lue_1,adr_lue_2,adr_ecrite,v_ecrite,ecrit,v_lue_1,v_lue_2) *)
            
let vide_circuit () =
  nb_tensions := 2 ;  (* 3+6*nb_bits ; *)
  assert(zero=0) ;
  assert(un=1) ;
  (* let _,_,_,_,e,_,_,_ = get_ram() in *)
  (* assert(e+1=!nb_tensions) ; *)
  portes_nand := [] ;
  portes_delai := [] ;
  meme_tension := []
  
                

let nand a b =
  let c = nouvelle_tension () in
  portes_nand := (a,b,c)::!portes_nand ;
  c

let delai a =
  let c = nouvelle_tension () in
  portes_delai := (a,c)::!portes_delai ;
  c

let relie a b =
  meme_tension := (a,b)::!meme_tension 

type porte =
  ZERO | UN | NAND | DELAI | ENTREE | UNDEFINED

let compacte_cc entrees sorties =

  (* On fait un algo de union-find sur les points de tensions pour
  trouver les points tensions vraiment différents *)

  let repr = Array.make (!nb_tensions) (-1) in
  let rec find x =
    if repr.(x) < 0
    then x
    else (repr.(x) <- find repr.(x) ; repr.(x))
  in
  let unite (a,b) =
    let a = find a and b = find b in
    let a,b = if repr.(a) < repr.(b) then (a,b) else (b,a) in
    repr.(a) <- repr.(a)+repr.(b) ;
    repr.(b) <- a 
  in
  List.iter unite (!meme_tension) ;

  (* Ensuite on fait une correspondance entre un point de tension et
  un numéro de composante connexe *)
  let nb_cc = ref 0 in

  let cc_de = Array.make (!nb_tensions) (-1) in
  for i = 0 to !nb_tensions-1 do
    if repr.(i) < 0
    then
      begin
        cc_de.(i) <- !nb_cc ;
        incr nb_cc ;
      end
  done ;
  for i = 0 to !nb_tensions-1 do
    cc_de.(i) <- cc_de.(find i) ;  
  done ;
  
  let portes_nand = List.rev_map (fun (a,b,c) -> cc_de.(a),cc_de.(b),cc_de.(c)) (!portes_nand) in
  let portes_delai = List.rev_map (fun (a,b) -> cc_de.(a),cc_de.(b)) (!portes_delai) in
  let entrees = Array.map (fun a -> cc_de.(a)) entrees in
  let sorties = Array.map (fun a -> cc_de.(a)) sorties in
  (!nb_cc,zero,un,portes_nand,portes_delai,entrees,sorties)

let print_circuit pnand pdelai =
  List.iter (fun (a,b,c) -> print_int c ; print_string " = NAND(" ; print_int a ; print_string "," ; print_int b; print_string ")\n") (!portes_nand) ;
  List.iter (fun (a,b) -> print_int b ; print_string " = DELAI(" ; print_int a; print_string ")\n") (!portes_delai) ;
  List.iter (fun (a,b) -> print_int a ; print_string " = " ; print_int b ; print_string "\n") (!meme_tension) ;
  List.iter (fun (a,b,c) -> print_int c ; print_string " = NAND(" ; print_int a ; print_string "," ; print_int b; print_string ")\n") (pnand) ;
  List.iter (fun (a,b) -> print_int b ; print_string " = DELAI(" ; print_int a; print_string ")\n") (pdelai) 
  
  
let type_points entrees nb_points portes_nand portes_delai =
  (* On vérifie que chaque point de tension correspond à exactement un de ces critères :
     - c'est zero ou un 
     - c'est la sortie d'une unique porte NAND 
     - c'est la sortie d'un délai
     - c'est une entrée
   *)
  let type_cc = Array.make nb_points UNDEFINED in
  let type_str = function
    | UNDEFINED -> "UNDEFINED"
    | NAND -> "NAND"
    | ZERO -> "0"
    | UN -> "1"
    | DELAI -> "DELAI"
    | ENTREE -> "ENTREE"
  in
  let assigne i t =
    match type_cc.(i) with
    | UNDEFINED ->  type_cc.(i) <- t
    | t' ->
       print_circuit portes_nand portes_delai ;
       failwith ("La tension "^(string_of_int i)^" est définie deux fois, comme "^(type_str t)^" et comme "^(type_str t'))
  in
  assigne zero ZERO ;
  assigne un UN ;
  List.iter (fun (_,_,c) -> assigne c NAND) portes_nand ;
  List.iter (fun (_,b) -> assigne b DELAI) portes_delai ;
  Array.iter (fun a -> assigne a ENTREE) entrees ;
  type_cc

let tri_topologique vient_apres =
  let rang = Array.map (fun _ -> -2) vient_apres in
  let index = Array.map (fun _ -> -2) vient_apres in
  let nb_places = ref (Array.length vient_apres) in
  
  let rec libere noeud =
    if rang.(noeud) = -1
    then failwith "Il n'existe pas d'ordre valable pour le circuit !" ;

    if rang.(noeud) < 0
    then
      begin
        rang.(noeud) <- -1 ;
        List.iter libere vient_apres.(noeud) ;
        decr nb_places ;
        rang.(noeud) <- !nb_places ;
        index.(!nb_places) <- noeud ;
      end
  in
  for i = 0 to Array.length vient_apres - 1 do
    libere i
  done ;
  (rang,index)
  

let compile entrees sorties =
  let nb_points, zero, un, portes_nand, portes_delai, entrees, sorties = compacte_cc entrees sorties in
  
  let _ = type_points entrees nb_points portes_nand portes_delai in

  (* Ensuite on vérifie qu'il existe un ordre sur les portes nand (si
    c=NAND(a,b) alors la CC de c doit être placée après celle de a et
    celle de b dans l'ordre que l'on créé. *)
  let vient_apres = Array.make nb_points [] in
  List.iter (fun (a,b,c) ->
      vient_apres.(a) <- c::vient_apres.(a) ;
      vient_apres.(b) <- c::vient_apres.(b)) portes_nand ;
  (* let l1,l2,_,_,_,v1,v2 = get_ram () in *)
  (* for i = 0 to nb_bits do *)
  (*   for j = 0 to nb_bits do *)
  (*     vient_apres.(l1.(i)) <- v1.(j)::vient_apres.(l1.(i)) ; *)
  (*     vient_apres.(l2.(i)) <- v2.(j)::vient_apres.(l2.(i)) ; *)
  (*   done  *)
  (* done *)
  let rang,_ = tri_topologique vient_apres in
  let portes_nand = List.sort (fun (_,_,c) (_,_,w) -> rang.(c)-rang.(w)) portes_nand in

  
  (*Enfin on peut simuler le circuit *)
  let valeurs_precedentes = Array.make nb_points 0 in
  let maj valeurs_entrees =
    let valeurs = Array.make nb_points 2 in
    valeurs.(zero) <- 0 ;
    valeurs.(un) <- 1 ;
    (* Gère les portes delai *)
    List.iter (fun (a,b) -> valeurs.(b) <- valeurs_precedentes.(a)) portes_delai ;
    (* Gère les entrées *)
    for i = 0 to Array.length entrees - 1 do
      valeurs.(entrees.(i)) <- valeurs_entrees.(i) ;
    done ;
    (* Gère les portes NAND *)
    List.iter (fun (a,b,c) -> valeurs.(c) <- (1-(valeurs.(a)*valeurs.(b)))) portes_nand ;
    (* Gère les portes NAND *)
    
    Array.iteri (fun i v -> valeurs_precedentes.(i) <- v) valeurs ;
    ((Array.map (fun i->valeurs.(i)) sorties))
    
  in
  vide_circuit () ;
  maj


let execute entrees valeurs sorties =
  let maj = compile entrees (Array.of_list sorties) in
  Array.to_list valeurs |> List.map maj |> List.map Array.to_list
  

let affiche_sorties res =
  List.iter (fun t ->
      List.iter (fun i->print_int i ; print_string " " ) t ;
      print_newline()) res

  
(* let _ =  *)

  
(*   let f1 a = nand a a in *)
            
(*   let f2 a b = f1 (nand a b) in *)
  
(*   let f3 a b = nand (f1 a) (f1 b) in *)
           
(*   let in1 = nouvelle_tension () in *)
(*   let in2 = nouvelle_tension () in *)
(*   let out1 = f3 in1 in2 in *)
(*   let out2 = f2 in1 in2 in *)
(*   let out3 = nand in1 in2 in *)
(*   let out4 = delai in1 in *)
(*   let out5 = delai in2 in *)
(*   let out6 = delai out1 in *)
(*   let out7 = delai out5 in *)
(*   let out8 = zero in *)
(*   let out9 = un in *)
(*   let res =   execute *)
(*                 [|in1;in2|] *)
(*                 [|[|0;0|];[|0;1|];[|1;0|];[|1;1|];[|0;0|];[|0;0|]|] *)
(*                 [in1;in2;out1;out2;out3;out4;out5;out6;out7;out8;out9] in *)
(*   (\* let _ =  *\) *)
(*   (\*     print_newline() ; *\) *)
(*   (\*     List.iter (fun t -> *\) *)
(*   (\*         List.iter (fun i->print_int i ; print_string " " ) t ; *\) *)
(*   (\*         print_newline()) res *\) *)
(*   (\* in *\) *)
(*   assert ( *)
(*       res *)
(*       = *)
(*         [(\*i i 1 2 3 4 5 6 7   *\) *)
(*           [0;0;0;0;1;0;0;0;0;0;1]; *)
(*           [0;1;1;0;1;0;0;0;0;0;1]; *)
(*           [1;0;1;0;1;0;1;1;0;0;1]; *)
(*           [1;1;1;1;0;1;0;1;1;0;1]; *)
(*           [0;0;0;0;1;1;1;1;0;0;1]; *)
(*           [0;0;0;0;1;0;0;0;1;0;1] *)
(*         ] *)
(*     ) *)
  
           
