open Mips
open Ast

let rec doublon l = match l with
  |[] -> []
  |e::f -> if List.mem e f then doublon f else e::(doublon f);; 

let new_line = [Li(A0 ,10); Li(V0 ,11); Syscall]
let end_code = [Label("end"); Li(V0 ,10); Syscall]

(* Exception � lever quand une variable est utilis�e sans �tre d�finie *)
exception VarUndef of string

let binop_to_arithop (bo: binop): arith = match bo with
  Add -> Add | Sub -> Sub | Mul -> Mul | Div -> Div

let ofset o = 
  Areg(o * -4, SP)

let ofset_of_var (lst: (string * int) list) (var: string): int = 
  List.fold_right (fun (v, o) acc -> if v = var then o else acc) lst 0 

let mem_var (lst: (string * int) list) (var: string): bool =
  List.fold_right (fun (v, o) acc -> v = var || acc) lst false

(* Compilation d'une expression *)	
let rec compile_expr (e: expr) (o: int) (let_in_vars: (string * int) list): instruction list= 
  match e with
    | Cst(i) -> [
      Li(T 0, i);
      Sw(T 0, ofset o)
    ]
    | Var(v) -> 
      if mem_var let_in_vars v then 
        let pos = ofset_of_var let_in_vars v in 
      [
          Lw(T 0, ofset pos);
          Sw(T 0, ofset o)
      ] else
      [
        Lw(T 0, Alab("Var_" ^ v));
        Sw(T 0, ofset o)
      ]
      (*    | Call(id, e) -> 
            (Arithi(Sub, SP, SP, 4 * (o + 1 + (List.length let_in_vars))))::(compile_expr e 0 []) @
            [
            Jal(id);
            Lw(T 0, ofset 1);
            Arithi(Add, SP, SP, 4 * (o + 1 + (List.length let_in_vars)));
            Sw(T 0, ofset o)
          ]
      *)
    | Binop(bo, e1, e2) ->
      (compile_expr e1 (o+1) let_in_vars) @ 
      (compile_expr e2 (o+2) let_in_vars) @ 
      [
        Lw(T 0,ofset (o+1)); 
        Lw(T 1, ofset (o+2)); 
        Arith(binop_to_arithop bo, T 0, T 0, T 1);
        Sw(T 0, ofset o)
      ]
      
    | Letin(v, e1, e2) -> 
      let new_lst = (v, o)::let_in_vars in
      (compile_expr e1 o let_in_vars) @ 
      (compile_expr e2 (o+1) new_lst) @ 
      [
        Lw(T 0, ofset (o+1));
        Sw(T 0, ofset o)
      ]
      
    | Call(id, e) -> 
      (Arithi(Sub, SP, SP, 4 * (o + 1 + (List.length let_in_vars))))::(compile_expr e 0 []) @
      [
      Jal(id);
      Lw(T 0, ofset 1);
      Arithi(Add, SP, SP, 4 * (o + 1 + (List.length let_in_vars)));
      Sw(T 0, ofset o)
    ]

(* Compilation d'un "statement" *)

let compile_instr = function
  | Print e -> 
    compile_expr e 0 [] @ [     
      Lw(A0, ofset 0);
      Li(V0 ,1);
      Syscall;
] @ new_line
  | Read x ->  [ 
    Li(V0 ,5);
    Syscall;
    Sw(V0, Alab("Var_" ^ x))
]
  | Function(id,arg,e) -> []
     
let compile_vars = function
  | Print e -> []
  | Read x ->  [Word("Var_" ^ x, 0)]
  | Function(id,arg,e) -> []

let compile_fonction = function
  | Print e -> []
  | Read x ->  []
  | Function(id,arg,e) -> [Label(id); Sw(RA, ofset (-1))] @ (compile_expr e 1 [(arg, 0)]) @ [Lw(RA, ofset (-1)); Jr(RA)]


(* Compile le programme p et enregistre le code dans le fichier ofile *)
let compile_program p ofile =
  let code = List.map compile_instr p |> List.concat in
  let vars = List.map compile_vars p |> List.concat in
  let functions = List.map compile_fonction p |> List.concat in
  let program_mips = 
    { text = 
    functions @ (Label("main")::code) @ end_code;
      data = doublon vars
    }
  in
  Mips.print_program program_mips ofile;
  
