open Circuit
open Logique
open Arithmetique

let rec pow a n = 
  if n = 0 then 1
  else a * (pow a (n-1))

let bit_registre doit_ecrire valeur_ecrite = 
  let data = nouvelle_tension () in
  relie (delai (mux doit_ecrire data valeur_ecrite)) data;
  data

let word_registre doit_ecrire valeur_ecrite = 
  let data = Array.init 16 (fun _ -> nouvelle_tension ()) in
  Array.iter2 (fun b bv -> relie (delai (mux doit_ecrire b bv)) b) data valeur_ecrite;
  data


 
let rec memoire taille_addr set l1 l2 e v =
  if taille_addr = 0 then
    let n1 = word_registre set v in    (n1, n1)
  else
    let mg1, mg2 = memoire (taille_addr-1) (et set (neg (Array.of_list e).(taille_addr-1))) l1 l2 e v in
    let md1, md2 = memoire (taille_addr-1) (et set ((Array.of_list e).(taille_addr-1))) l1 l2 e v in
    (selecteur ((Array.of_list l1).(taille_addr-1)) mg1 md1 , selecteur ((Array.of_list l2).(taille_addr-1)) mg2 md2)

let rom l1 l2 valeurs =
  let rec aux a1 a2 i k =
    match a1, a2 with
    |([], _) | (_, []) -> let n1 = (if i < (Array.length valeurs) then valeurs.(i) else (Array.make 16 zero)) in    (n1, n1)
    |(e1::q1, e2::q2) ->
      let mg1, mg2 = aux q1 q2 i (k+1) in
      let md1, md2 = aux q1 q2 (i + (pow 2 k)) (k+1) in
      (selecteur e1 mg1 md1 , selecteur e2 mg2 md2)
    in aux l1 l2 0 0


let ram_rom taille_addr set l1 l2 e v contenu_rom =
  let r1, r2 = rom l1 l2 contenu_rom in
  let m1, m2 = memoire taille_addr set l1 l2 e v in
  (selecteur (est_positif (somme (nb_to_array_t (Array.length contenu_rom)) (oppose (Array.of_list l1)))) (r1) (m1),
   selecteur (est_positif (somme (nb_to_array_t (Array.length contenu_rom)) (oppose (Array.of_list l2)))) (r2) (m2))