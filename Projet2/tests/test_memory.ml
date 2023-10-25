open Circuit
open Arithmetique
open Memory 

let _test_bit_registre =
  let inp = nouvelle_tension () in
  let flag = nouvelle_tension () in
  let out = bit_registre flag inp in
  let res = compile [|flag;inp|] [|out|] in

  let tests = [
      [|1;0|]; (* set 0 *)
      [|1;0|];
      [|0;0|];
      [|0;1|];
      [|0;0|];
      [|0;1|];
      [|1;1|]; (* set 1 *)
      [|1;0|]; (* set 0 *)
      [|1;1|]; (* set 1 *)
      [|0;0|];
      [|0;1|];
      [|0;0|];
      [|1;0|]; (* set 0 *)
      [|0;1|]
    ] in
  let expected = [
      [|0|];
      [|0|];
      [|0|];
      [|0|];
      [|0|];
      [|0|];
      [|0|];
      [|1|];
      [|0|];
      [|1|];
      [|1|];
      [|1|];
      [|1|];
      [|0|]
    ] in
  List.iter2 (fun test exp -> assert (res test = exp)) tests expected

let _test_word_registre =
  let inp = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
  let flag = nouvelle_tension () in
  let out = word_registre flag inp in
  let res = compile (Array.concat [[|flag|];inp]) out in
  let tests = [
      1,42 ;
      1, 1 ;
      0, 3000 ;
      1, 32767 ;
      0, 65535 ;
      1, 0
    ] in
  let tests = tests @ (List.map (fun (x,y) -> (1-x,y)) tests) in
  let contenu = ref 0 in
  List.iter (
      fun (flag,value) ->
      assert(Array.concat [[|flag|] ; nb_to_array value] |> res = nb_to_array !contenu ) ;
      if flag=1 then contenu := value 
    ) tests
           

let _test_memoire_avec_une_lecture_n_bits =
  let rec powm x p m = (* compute x^p mod m *)
    if p = 0
    then 1
    else
      let v = powm ((x*x) mod m) (p/2) m in
      if p mod 2 = 1
      then (x*v mod m)
      else v
  in

  for k = 4 to 8 do
    let adr_lue = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
    let adr_lue2 = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
    let adr_ecrite = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
    let valeur_ecrite = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
    let flag = nouvelle_tension () in
    let nb_mem = powm 2 k 100000 in
    let out1,out2 = memoire k flag
                            (Array.to_list adr_lue)
                            (Array.to_list adr_lue2)
                            (Array.to_list adr_ecrite)
                            valeur_ecrite in
    let res =   compile (Array.concat [[|flag|];adr_lue;adr_lue2;adr_ecrite;valeur_ecrite]) (Array.concat [out1; out2]) in
    
    
    let tests = List.init 1000 (fun j -> let i = j/2 in (j mod 2),
                                                        powm 3 i nb_mem,
                                                        powm 5 i nb_mem,
                                                        powm 7 i nb_mem,
                                                        (3*j) mod 65536) in
    let contenu = Array.make nb_mem 0 in
    List.iter (fun (x,y,z,w,v) ->
        let input = Array.of_list (x::nb_vers_bits y@nb_vers_bits z@nb_vers_bits w@nb_vers_bits v) in
        assert(x=0 || x=1) ;
        let cur = res input in
        assert (cur = Array.concat [nb_to_array contenu.(y) ; nb_to_array contenu.(z) ]) ;
        if x=1 then contenu.(w) <-  v 
      ) tests
  done

let _test_rom =
  let l1 = List.init nb_bits (fun _ -> nouvelle_tension ()) in
  let l2 = List.init nb_bits (fun _ -> nouvelle_tension ()) in
  let t = 1024 in
  let contenu = Array.init t
                           (fun i -> (i*i) mod 65536 |>
                                       nb_vers_bits |>
                                       List.map (function | 0 -> zero | _ -> un) |>
                                       Array.of_list) in
  let out1,out2 = rom l1 l2 contenu in
  let out = Array.concat [out1;out2] in
  let test =
    let s = compile (List.map Array.of_list [l1;l2] |> Array.concat) out in
    fun a1 a2 ->
    assert(
        s (Array.concat [nb_vers_bits a1 |> Array.of_list; nb_vers_bits a2 |> Array.of_list])
        =
          Array.concat [nb_vers_bits (a1*a1 mod 65536) |> Array.of_list;
                        nb_vers_bits (a2*a2 mod 65536) |> Array.of_list]
      )
  in
  test 10 100 ;
  test 0 1 ;
  test 1 0 ;
  test 100 (t-1) ;
  test (t-1) 0

  

let _ =
  print_string "Tous les tests mémoires sont passés !\n"
