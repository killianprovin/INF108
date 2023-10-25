open Circuit ;;
open Logique ;;
open Arithmetique ;;

                 
let _test_neg =
  let inp = nouvelle_tension () in
  let out = neg inp in
  let res =   compile [|inp|] [|out|] in
  assert (res [|0|] = [|1|] ) ;
  assert (res [|1|] = [|0|] ) ;
  assert (res [|1|] = [|0|] ) ;
  assert (res [|0|] = [|1|] )


           
let _test_ou =
  let in1 = nouvelle_tension () in
  let in2 = nouvelle_tension () in
  let out = ou in1 in2 in
  let res = compile [|in1; in2|] [|out|] in
  assert(res [|0;0|] = [|0|] ) ;
  assert(res [|1;0|] = [|1|] ) ;
  assert(res [|0;1|] = [|1|] ) ;
  assert(res [|1;1|] = [|1|] ) ;
  assert(res [|0;0|] = [|0|] )


let _test_xor =
  let in1 = nouvelle_tension () in
  let in2 = nouvelle_tension () in
  let out = xor in1 in2 in
  let res = compile [|in1;in2|] [|out|] in
  assert (res [|0;0|] = [|0|] ) ;
  assert (res [|1;0|] = [|1|] ) ;
  assert (res [|0;1|] = [|1|] ) ;
  assert (res [|1;1|] = [|0|] ) ;
  assert (res [|0;0|] = [|0|] )

           
let _test_et =
  let in1 = nouvelle_tension () in
  let in2 = nouvelle_tension () in
  let out = et in1 in2 in
  let res = compile [|in1;in2|] [|out|] in
  assert (res [|0;0|] = [|0|] ) ;
  assert (res [|1;0|] = [|0|] ) ;
  assert (res [|0;1|] = [|0|] ) ;
  assert (res [|1;1|] = [|1|] ) ;
  assert (res [|0;0|] = [|0|] )
  
  
let _test_mux =
  let flag = nouvelle_tension () in
  let in1 = nouvelle_tension () in
  let in2 = nouvelle_tension () in
  let out = mux flag in1 in2 in
  let res =   compile [|in1;in2;flag|] [|out|] in
  assert( res [|0;0;0|] = [|0|] ) ;
  assert( res [|0;1;0|] = [|0|] ) ;
  assert( res [|1;0;0|] = [|1|] ) ;
  assert( res [|1;1;0|] = [|1|] ) ;
  assert( res [|0;0;1|] = [|0|] ) ;
  assert( res [|0;1;1|] = [|1|] ) ;
  assert( res [|1;0;1|] = [|0|] ) ;
  assert( res [|1;1;1|] = [|1|] ) ;
  assert( res [|0;0;0|] = [|0|] ) 
  
let _test_inverse =
  let inp = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
  let out = inverse inp in
  let res = compile inp out in
  let t = Array.make nb_bits 0 in
  let inv = Array.map (fun v->1-v) in
  for i = 0 to 100 do
    t.( (i*i) mod nb_bits ) <- (i mod 2) ;
    assert(res t = inv t)
  done

let _test_selecteur =
  let in1 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let in2 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let flag = nouvelle_tension () in
  let out = selecteur flag in1 in2 in
  let tests = [
      42,64 ;
      1,3 ;
      3000,8102 ;
      32767,32768 ;
      32768,32768 ;
      0,32768 ;
      1,32767 ;
    ] in
  let tests = (List.map (fun (x,y) -> (0,x,y)) tests) @
                (List.map (fun (x,y) -> (1,x,y)) tests) in
  let res =   compile (Array.concat [in1;in2;[|flag|]]) out in
  List.iter (fun (f,x,y) -> assert(Array.concat [nb_to_array x;nb_to_array y;[|f|]] |> res = nb_to_array (if f=0 then x else y))) tests
  

let test_fun f_real f_test =
  let in1 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let in2 = Array.init (nb_bits) (fun _ -> nouvelle_tension ()) in
  let out = f_test in1 in2 in
  let tests = [
      42,64 ;
      1,3 ;
      3000,8102 ;
      32767,32768 ;
      32768,32768 ;
      0,32768 ;
      1,32767 ;
    ] in
  let tests = (List.map (fun (x,y) -> (nb_to_array x,nb_to_array y)) tests) in
  let tests = List.map (fun (x,y) -> (Array.concat [x;y]),(Array.map2 f_real x y)) tests in
  let res =   compile (Array.concat [in1;in2]) out in
  List.iter (fun (inp,exp) -> assert(res inp = exp)) tests

let _test_etl =
  test_fun (fun a b -> if a+b =2 then 1 else 0) et_logique

let _test_oul =
  test_fun (fun a b -> if a+b > 0 then 1 else 0) ou_logique 

let _test_xor =
  test_fun (fun a b -> if a+b =1 then 1 else 0) xor_logique 

let _ =
  print_string "Tous les tests logiques sont passés !\n"
