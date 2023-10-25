open Logique
open Circuit
open Arithmetique
open Memory
open Alu
  
let ( ++ ) a1 a2 = Array.append a1 a2 
let array_zero i = Array.make i zero
let relie_bitabit = Array.iter2 relie


let cpu program =
  
  (* registre pc = instruction en cours *)
  let nouvelle_valeur_pc = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
  let pc = Array.map delai nouvelle_valeur_pc in

  (* def de la mémoire RAM/ROM *)
  let ecrit_en_RAM = nouvelle_tension () in
  let charge_ram = Array.init nb_bits (fun _ -> nouvelle_tension ()) in 
  let adr_ecrit_ram = Array.init nb_bits (fun _ -> nouvelle_tension ()) in 
  let val_ecrit_ram = Array.init nb_bits (fun _ -> nouvelle_tension ()) in 
  let instruction,lu_ram = ram_rom 8
                        ecrit_en_RAM
                        (Array.to_list pc)
                        (Array.to_list charge_ram)
                        (Array.to_list adr_ecrit_ram)
                        val_ecrit_ram
                        program in

  (* premier décodage de l'instruction (sur 16 bits) *)
  let ins = Array.sub instruction 0 4 in
  let r1 = Array.sub instruction 4 4 in
  let r2 = Array.sub instruction 8 4 in
  let r3 = Array.sub instruction 12 4 in
  let val_r2 = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let val_r3 = Array.init nb_bits (fun _ -> nouvelle_tension()) in

  (* def de la mémoire registres *)
  let ecrire_reg = nouvelle_tension () in
  let valeur_ecrire_registre = Array.init nb_bits (fun _ -> nouvelle_tension ()) in
  let registre_2,registre_3 = memoire 4 ecrire_reg (Array.to_list r2) (Array.to_list r3) (Array.to_list r1) valeur_ecrire_registre in


  let r2r3 = r2 ++ r3 in
  let r1r2r3 = r1 ++ r2 ++ r3 in
  (* Il faut maintenant faire tous les branchements *)


  (* appel à l'ALU instruction 8 à 15 *)
  let res_alu = alu ins registre_2 registre_3 in

  (* instructions constantes 6 et 7 *)
  let res_op6 =  Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let res_op7 =  Array.init nb_bits (fun _ -> nouvelle_tension()) in
  for i=0 to 7 do
    relie res_op6.(i) r2r3.(i);
    relie res_op7.(i + 8) r2r3.(i);
  done;

  (* lit ram instruction 5 *)
  relie_bitabit charge_ram registre_2;
  let res_op5 = lu_ram in

  (* lit pc instruction 4 *)

  let res_op4 = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let v = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  for i=0 to 6 do
    relie v.(i) r2r3.(i);
  done;
  relie v.(15) r2r3.(7);
  relie_bitabit res_op4 (somme v pc);

  (*on relie les opcode de 4 a 15*)
  let res_null = Array.init nb_bits (fun _ -> nouvelle_tension()) in (*si op est 0, 1, 2 ou 3*)
  let res_op4a15 = 
  selecteur (ins.(3)) 
    (selecteur (ins.(2))
      (res_null)
      (selecteur (ins.(1))
        (selecteur (ins.(0))
          (res_op4)
          (res_op5)
        )
        (selecteur (ins.(0))
          (res_op6)
          (res_op7)
        ) 
      ) 
    ) 
    (res_alu) in
  let ecrire_reg_op4a15 = 
    mux (ins.(3)) 
      (mux (ins.(2))
        (zero)
        (un) 
      ) 
      (un) in
      let offset16 = Array.init nb_bits (fun _ -> nouvelle_tension()) in
      for i=0 to 8 do
        relie offset16.(i) r1r2r3.(i + 2);
      done;
      relie offset16.(15) r1r2r3.(11);
      let taille_jum = 
        selecteur (ins.(3)) 
        (selecteur (ins.(2))
          (selecteur (ins.(1))
            (nb_to_array_t 1)
            (selecteur (ins.(0))
              (offset16)
              (nb_to_array_t 1)
            )
          )
          ((nb_to_array_t 1)) 
        )
        (nb_to_array_t 1)
      in
      relie_bitabit nouvelle_valeur_pc (somme pc taille_jum);
  relie_bitabit valeur_ecrire_registre res_op4a15;
  relie ecrire_reg ecrire_reg_op4a15;
  
  (* ecrit ram, instruction 3  *)
  let adr = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  let offset = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  for i=0 to 2 do
    relie offset.(i) r1.(i);
  done;
  relie offset.(15) r1.(3);
  relie_bitabit adr_ecrit_ram (somme (registre_2) offset);
  relie_bitabit val_ecrit_ram registre_3;


  (* JUMPs *)

  (* Pour les jump on a :
     ins = 0 -> est_nul 
     ins = 1 -> est_neg
     ins = 2 -> inconditionnel
     
     r0.(0) définit si relatif
     r0.(1) définit si inverse le cas de jump

     dans le cas conditionnel :
         *r3 le registre si pas relatif
         pc + r3 si relatif avec r3 complémenté à deux
     dans le cas inconditionnel 
         pc + r1[2]r1[3]r2r3 ou  r1[2]r1[3]r2r3
  *)

  (* unconditional JUMP instruction 2 *)

  let offset16 = Array.init nb_bits (fun _ -> nouvelle_tension()) in
  for i=0 to 8 do
    relie offset16.(i) r1r2r3.(i + 2);
  done;
  relie offset16.(15) r1r2r3.(11);
  let taille_jum = 
    selecteur (ins.(3)) 
    (selecteur (ins.(2))
      (selecteur (ins.(1))
        (nb_to_array_t 1)
        (selecteur (ins.(0))
          (offset16)
          (nb_to_array_t 1)
        )
      )
      ((nb_to_array_t 1)) 
    )
    (nb_to_array_t 1)
  in
  relie_bitabit nouvelle_valeur_pc (somme pc taille_jum);

  (* conditionnal JUMP, cond=r2, dest=r3 *)
  let pc_cond = nouvelle_tension () in

  (* Pour pouvoir observer le comportement du CPU je vous conseille de
     renvoyer les champs suivants *)
  (instruction,val_r2,val_r3,pc,pc_cond,lu_ram)

let ins_add x y dst =  8+16*(dst+16*(x+16*y))
let ins_moins x y dst =  12+16*(dst+16*(x+16*y))
let ins_et x y dst =  10+16*(dst+16*(x+16*y))
let ins_ou x y dst =  14+16*(dst+16*(x+16*y))
let ins_inv x dst =  9+16*(dst+16*(x+16*0))
let ins_xor x y dst =  13+16*(dst+16*(x+16*y))
let ins_inc x dst =  11+16*(dst+16*(x+16*0))
let ins_dec x dst =  15+16*(dst+16*(x+16*0))
let ins_cst_hi valeur dst = 7+16*(dst+16*(valeur))
let ins_cst_low valeur dst = 6+16*(dst+16*(valeur))
let ins_load reg_adr delta dst =5+16*(dst+16*(reg_adr+16*(if delta < 0 then delta+16 else delta)))
let ins_get_pc dst delta = 4 + 16*(dst+16*(if delta < 0 then delta+256 else delta))
let ins_store reg_adr delta reg_val =3+16*((if delta < 0 then delta+16 else delta)+16*(reg_adr+16*reg_val))
let ins_jump_short delta = 2+64*(delta)
let ins_jump_short_relatif delta = 2+16+64*(if delta < 0 then delta+2048 else delta)
let ins_jump_negatif_relatif reg_cond delta =
  let delta = if delta < 0 then delta+128 else delta in
  1+16+32*(delta mod 8 + 8*(reg_cond+16*(delta/8)))
let ins_jump_est_zero_relatif reg_cond delta =
  let delta = if delta < 0 then delta+64 else delta in
  0+16+64*(delta mod 4 + 4*(reg_cond+16*(delta/4)))
let ins_jump_nonzero_relatif reg_cond delta =
  let delta = if delta < 0 then delta+64 else delta in
  0+16+32+64*(delta mod 4 + 4*(reg_cond+16*(delta/4)))
    
