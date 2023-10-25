open Cpu
open Circuit
open Arithmetique

let _test_cpu =
  let program = Array.map
                  (fun i -> nb_vers_bits i |>
                              List.map (function |0 -> zero | _ -> un)  |>
                              Array.of_list)
                          [|
                            ins_cst_low 15 1;
                            ins_cst_hi 15 2;
                            ins_cst_hi 15 3;
                            ins_add 1 2 1 ;
                            ins_xor 3 1 1 ;
                            ins_dec 1 1 ;
                            ins_jump_nonzero_relatif 1 (-1) ;
                            ins_cst_low 7 2 ; (* 7 *)
                            ins_inc 2 3 ;
                            ins_cst_hi 1 1 ;
                            ins_cst_low 42 4 ;
                            ins_store 1 0 4 ; (* 11 *)
                            ins_xor 4 5 6 ;
                            ins_load 1 0 5 ;
                            ins_moins 4 5 6 ;
                            ins_jump_short_relatif 0
                          |] in
  let mem,r2,r3,pc,pc_cond,lu_ram = cpu program in
  let s = compile [||] (Array.concat [mem;r2;r3;pc;[|pc_cond|];lu_ram]) in
  for _i = 0 to 50 do
    let t = s [||] in
    Array.iteri (fun i v -> if i < 16 then print_int v ) t ;
    print_string " r2=" ;
    Array.sub t 16 16 |> Array.to_list |> bit_liste_vers_nb |> print_int ;
    print_string " r3=" ;
    Array.sub t 32 16 |> Array.to_list |> bit_liste_vers_nb |> print_int ;
    print_string " pc=" ;
    Array.sub t 48 16 |> Array.to_list |> bit_liste_vers_nb |> print_int ;
    (* print_string " lu ram=" ; *)
    (* Array.sub t 65 16 |> Array.to_list |> bit_liste_vers_nb |> print_int ; *)
    print_string " cond=" ;
    print_int t.(64);
    print_string "\n" ;
  done ;
  print_newline () ;
  print_newline () ;
  print_newline () ;
  print_newline () ;
  print_newline () 
    
