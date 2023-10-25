open Circuit
open Arithmetique
  
let _test_half_adder =
  let in1 = nouvelle_tension () in
  let in2 = nouvelle_tension () in
  let out1,out2 = half_adder in1 in2 in
  let res = compile [|in1;in2|] [|out1;out2|] in
  assert (res [|0;0|] = [|0;0|] ) ;
  assert (res [|0;1|] = [|0;1|] ) ;
  assert (res [|1;0|] = [|0;1|] ) ;
  assert (res [|1;1|] = [|1;0|] ) ;
  assert (res [|0;0|] = [|0;0|] )
  

let _test_full_adder =
  let in1 = nouvelle_tension () in
  let in2 = nouvelle_tension () in
  let in3 = nouvelle_tension () in
  let out1,out2 = full_adder in1 in2 in3 in
  let res = compile [|in1;in2;in3|] [|out1;out2|] in
  assert (res [|0;0;0|] = [|0;0|] ) ;
  assert (res [|0;1;0|] = [|0;1|] ) ;
  assert (res [|1;0;0|] = [|0;1|] ) ;
  assert (res [|1;1;0|] = [|1;0|] ) ;
  assert (res [|0;0;1|] = [|0;1|] ) ;
  assert (res [|0;1;1|] = [|1;0|] ) ;
  assert (res [|1;0;1|] = [|1;0|] ) ;
  assert (res [|1;1;1|] = [|1;1|] ) ;
  assert (res [|0;0;0|] = [|0;0|] ) 

let _test_somme =
  let in1 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let in2 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out = somme in1 in2 in
  let tests = [
      42,64 ;
      1,3 ;
      3000,8102 ;
      32767,32768 ;
      32768,32768 ;
      0,32768 ;
      1,32767 ;
    ] in
  let tests = tests @ (List.map (fun (x,y) -> (y,x)) tests) in
  let res =   compile (Array.concat [in1;in2]) out in
  List.iter (fun (x,y) -> assert(Array.concat [nb_to_array x;nb_to_array y] |> res = nb_to_array (x+y))) tests


let _test_increment =
  let inp = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out = increment inp in
  let res =  compile inp out in
  let tests = [42 ; 1 ; 3000 ; 32767 ; 65535 ; 0 ] in
  List.iter (fun i -> assert( nb_to_array i |> res =  nb_to_array (i+1))) tests
  
let _test_increment =
  let inp = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out = decrement inp in
  let res =  compile inp out in
  let tests = [42 ; 1 ; 3000 ; 32767 ; 65535 ; 0 ] in
  List.iter (fun i -> assert( nb_to_array i |> res =  nb_to_array (i-1))) tests


let _test_difference =
  let in1 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let in2 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out = difference in1 in2 in
  let tests = [
      42,64 ;
      1,3 ;
      3000,8102 ;
      32767,32768 ;
      32768,32768 ;
      0,32768 ;
      1,32767 ;
    ] in
  let tests = tests @ (List.map (fun (x,y) -> (y,x)) tests) in
  let res =   compile (Array.concat [in1;in2]) out in
  List.iter (fun (x,y) -> assert(Array.concat [nb_to_array x;nb_to_array y] |> res = nb_to_array (x-y))) tests

let _test_nul =
  let in1 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out1 = est_nul in1 in
  let tests = [0;1;2;3;42;-1;-2;-3;32767;-32768] in
  let res = compile in1 [|out1|] in
  List.iter (fun i -> assert( nb_to_array i |> res = [|if i=0 then 1 else 0|])) tests

let _test_positif =
  let in1 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out1 = est_positif in1 in
  let tests = [0;1;2;3;42;-1;-2;-3;32767;-32768] in
  let res = compile in1 [|out1|] in
  List.iter (fun i -> assert( nb_to_array i |> res = [|if i>=0 then 1 else 0|])) tests

let _ =
  print_string "Tous les tests arithmétiques sont passés !\n"
  
