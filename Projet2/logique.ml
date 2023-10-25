open Circuit

let neg a = nand a a
let ou a b = nand (neg a) (neg b)
let et a b = neg (nand a b)
let xor a b = ou (et a (neg b)) (et b (neg a))
let mux flag a b = ou (et a (neg flag)) (et b flag)


(* Les fonctions suivantes prennent en entrée des tableau *)
let inverse a = Array.map neg a

let selecteur flag a b = Array.map2 (mux flag) a b 

let et_logique a b = Array.map2 et a b 
  
let ou_logique a b = Array.map2 ou a b 

let xor_logique a b = Array.map2 xor a b 