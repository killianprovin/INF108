(* Production de code pour notre langage *)

open Mips
open Ast


let new_line = [Li(A0 ,10); Li(V0 ,11); Syscall]
let end_code = [Syscall; Label("end"); Li(V0 ,10); Syscall]

(* Exception � lever quand une variable est utilis�e sans �tre d�finie *)
exception VarUndef of string


let global_var = Hashtbl.create 1
let new_pos = ref 0

(* Compilation d'une expression *)	
let rec compile_expr (e: expr): instruction list= 
  match e with
    | Cst(i) -> []
    | Var(v) -> []
    | Binop(bo, e1, e2) -> []
    | Letin(v, e1, e2) -> []
    | Call(v, e1) -> []

(* Compilation d'une instruction *)            
let compile_instr = function
  | Print Var(x) -> 
    let pos = if (Hashtbl.mem global_var x) then (Hashtbl.find global_var x) else (Hashtbl.add global_var x !new_pos; incr new_pos; !new_pos - 1) in
    [

    Arithi(Sub, GP, GP, 4*pos);
    Lw(A0, Areg(0, GP));
    Arithi(Add, GP, GP, 4*pos);
    Li(V0 ,1);
    Syscall;

  ]@new_line
  | Read x ->  
    let pos = if (Hashtbl.mem global_var x) then (Hashtbl.find global_var x) else (Hashtbl.add global_var x !new_pos; incr new_pos; !new_pos - 1) in
    [

    Li(V0 ,5);
    Syscall;
    Move(A0, V0);
    Arithi(Sub, GP, GP, 4*pos);
    Sw(A0, Areg(0, GP));
    Arithi(Add, GP, GP, 4*pos)

  ]
     

(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map compile_instr p |> List.concat in
  let p = 
    { text = 
        (Label("main")::code) @ end_code;
      data = [
          Asciiz("Test", "Bonjour")
        ]
    }
  in
  Mips.print_program p ofile;
