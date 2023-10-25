open Circuit
open Arithmetique
open Alu


  
let _test_alu =
  let instruction = Array.init 3 (fun _ -> nouvelle_tension ()) in
  let x = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
  let y = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
  let out = alu instruction x y in
  let circuit = compile (Array.concat [instruction;x;y]) out in
  let test expected (a,b,c,x,y) =
    assert(
          circuit (Array.of_list (a::b::c::nb_vers_bits x@nb_vers_bits y)) |> Array.to_list
        =
          nb_vers_bits expected
          ) in
  (* test plus *)
  test (42+12)   (0,0,0,42,12);
  test (0+60000) (0,0,0,0,60000);
  test (27+2333) (0,0,0,27,2333);
  (* test moins *)
  test (42-12)   (0,0,1,42,12);
  test (0-60000) (0,0,1,0,60000);
  test (27-2333) (0,0,1,27,2333);
  (* test   &   *)
  test (42 land 12)   (0,1,0,42,12);
  test (0 land 60000) (0,1,0,0,60000);
  test (27 land 2333) (0,1,0,27,2333);
  (* test   |   *)
  test (42 lor 12)    (0,1,1,42,12);
  test (0 lor 60000)  (0,1,1,0,60000);
  test (27 lor 2333)  (0,1,1,27,2333);
  (* test   inv *)
  test (lnot 42) (1,0,0,42,12);
  test (lnot 0)  (1,0,0,0,60000);
  test (lnot 27) (1,0,0,27,2333);
  (* test   xor *)
  test (42 lxor 12)   (1,0,1,42,12);
  test (0 lxor 60000) (1,0,1,0,60000);
  test (27 lxor 2333) (1,0,1,27,2333);
  (* test inc   *)
  test (42+1) (1,1,0,42,12);
  test (0+1)  (1,1,0,0,60000);
  test (27+1) (1,1,0,27,2333);
  (* test dec   *)
  test (42-1) (1,1,1,42,12);
  test (0-1)  (1,1,1,0,60000);
  test (27-1) (1,1,1,27,2333)

  
let _ =
  print_string "Tous les tests ALU sont passés !\n"
