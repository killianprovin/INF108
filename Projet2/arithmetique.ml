open Circuit
open Logique

let vers_bus b =
  Array.init nb_bits (fun i -> if i=0 then b else zero)

let half_adder a b = (et a b, xor a b)
  
let full_adder a b c = let r_ab, s_ab = half_adder a b in (mux r_ab (et s_ab c) r_ab, xor s_ab c)

let somme a b = let r = ref zero in Array.map2 (fun ba bb -> let r1, s = full_adder ba bb !r in r := r1; s) a b

let increment a = somme a (vers_bus un)

let oppose a = increment (inverse a) 

let decrement a = somme a (oppose (vers_bus un))

let difference a b = somme a (oppose b)

let est_nul a = neg (Array.fold_left (fun acc b -> ou acc b) zero a)

let est_negatif a = a.((Array.length a) - 1)

let est_positif a = neg (est_negatif a)

let int_to_tension = function
  | 1 -> un
  | 0 -> zero
  | _ -> failwith "Impossible"
  
let rec bit_liste_vers_nb = (* Suppose le petit-boutisme*)
  function 
  | [] -> 0
  | a::q -> a+2*(bit_liste_vers_nb q)

  let nb_vers_bits n = (* Suppose le petit-boutisme *)
  let n = if n < 0 then n + 65536 else n in
  let rec foo n b =
    if b = 0
    then []
    else (n mod 2)::(foo (n/2) (b-1))
  in
  foo n nb_bits
  
let nb_to_array i = ((i+65536) mod 65536) |> nb_vers_bits |> Array.of_list

let nb_vers_bits_t n = (* Suppose le petit-boutisme *)
  let n = if n < 0 then n + 65536 else n in
  let rec foo n b =
    if b = 0
    then []
    else (int_to_tension (n mod 2))::(foo (n/2) (b-1))
  in
  foo n nb_bits
  
let nb_to_array_t i = ((i+65536) mod 65536) |> nb_vers_bits_t |> Array.of_list

