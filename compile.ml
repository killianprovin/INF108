(* Production de code pour notre langage *)

open Mips
open Ast

exception VarUndef of string;;
exception Error of string ;;
exception RuntimeError of string * Lexing.position ;;

(*fonction permetant l'affichage d'erreur avec la possition*)
let create_store () =
  let data = ref None in
  let get () = match !data with | Some x -> x | None -> failwith "Position Invalide" in
  let set x = data := Some x in
  (get,set)
let get_pos, set_pos = create_store ()
let erreur txt= raise (RuntimeError(txt, get_pos()))
(*variable permetant de distinguer les differants block if*)
let index_if = ref 0



let new_line = [Li(A0 ,10); Li(V0 ,11); Syscall]
let end_code = [Label("end"); Li(V0 ,10); Syscall]




(*fonction qui calcule la possition d'un element dans une liste*)
let pos_lst l e = 
  let rec pos_lst_rec p lst elt = match lst with [] -> erreur "Possition pile invalide" | ei::_ when elt=ei -> p | _::q -> pos_lst_rec (p+1) q elt in
  pos_lst_rec 0 l e

let binop_to_arithop (b: binop): arith = match b with
  | Add -> Add | Sub -> Sub | Mul -> Mul | Div -> Div 
  | And -> And | Or -> Or | Le -> Slt | Nor -> Nor
  | _ -> erreur "Non implementer"

let ofset o = Areg(o * -4, SP)

let rec compile_call s lst_var lst_e o = match (s, lst_e) with 
  | ("print_int", [e]) -> (compile_expr lst_var (List.hd lst_e) o) @ 
    [     
      Lw(A0, ofset (List.length lst_var));
      Li(V0 ,1);
      Syscall;
    ] @ new_line
  | (f, args) ->
    (*on stock les differants element les un a la suite des autres*)
    ((List.mapi (fun i e -> compile_expr lst_var e (1 + i + o)) args) |> List.concat) @
    [
      Arithi(Sub, SP, SP, 4 * o);
      Jal(f);
      Arithi(Add, SP, SP, 4 * o);
      Sw(T 0, ofset o)
    ]

and compile_expr lst_var e o = match e with
  | Const(i) -> [Li(T 0, i); Sw(T 0, ofset o)]
  | Val(Var(v)) -> if List.mem v lst_var then
                    [
                        Lw(T 0, ofset (pos_lst lst_var v));
                        Sw(T 0, ofset o)
                    ]
                   else erreur "Variable inexistante"
  | UnOp(e1) -> compile_expr lst_var (BinOp(Sub, Const 1, e1)) o
  | BinOp(b, e1, e2) -> (match b with 
    | Mod -> (
      (compile_expr lst_var e1 (o+1)) @ 
      (compile_expr lst_var e2 (o+2)) @ 
      [
        Lw(T 0, ofset (o+1)); 
        Lw(T 1, ofset (o+2)); 
        Mod(T 0, T 1);
        Mfhi(T 0);
        Sw(T 0, ofset o);
      ])
    | Neq -> compile_expr lst_var (BinOp(Or , BinOp(Le, e1, e2), BinOp(Le, e2, e1))) o
    | Eqq -> compile_expr lst_var (UnOp(BinOp(Neq, e1, e2))) o
    | Leq -> compile_expr lst_var (BinOp(Or, BinOp(Eqq,e1,e2), BinOp(Le,e1,e2))) o
    | Geq -> compile_expr lst_var (UnOp(BinOp(Le,e1,e2))) o
    | Ge -> compile_expr lst_var (UnOp(BinOp(Leq,e1,e2))) o
    | _ -> (
      (compile_expr lst_var e1 (o+1)) @ 
      (compile_expr lst_var e2 (o+2)) @ 
      [
        Lw(T 0, ofset (o+1)); 
        Lw(T 1, ofset (o+2)); 
        Arith(binop_to_arithop b, T 0, T 0, T 1);
        Sw(T 0, ofset o);
      ]))
  | Ecall(s, lst_e) -> compile_call s lst_var lst_e o

let rec compile_stmt lst_var (stmt, pos) nom_fonction = 
  set_pos pos;
  match stmt with
  | Sblock(b) -> List.fold_left (fun (acc, vars) s -> (let c_s, new_vars = compile_stmt vars s nom_fonction in acc@c_s, new_vars)) ([], lst_var) b
  | Sif(e, a1, a2) -> let ind = !index_if in (incr index_if); 
    (compile_expr lst_var e (List.length lst_var)) @ [
    Lw(T 0, ofset (List.length lst_var));
    Beq(T 0, ZERO ,Alab("else_" ^ string_of_int ind))] @
    (let c_s, _ = compile_stmt lst_var a1 nom_fonction in c_s) @ [
    J("endif_" ^ string_of_int ind);
    Label("else_" ^ string_of_int ind)] @ 
    (let c_s, _ = compile_stmt lst_var a2 nom_fonction in c_s) @ [
    Label("endif_" ^ string_of_int ind)], lst_var
  | Sreturn(e) -> (compile_expr lst_var e (List.length lst_var)) @ [Lw(T 0, ofset (List.length lst_var)); J(if nom_fonction="main" then "end" else "return_"^nom_fonction)], lst_var
  | Dec(Var(s)) -> [], lst_var @ [s]
  | Sassign(Var(s), e) -> (compile_expr lst_var e (List.length lst_var)) @ [Lw(T 0, ofset (List.length lst_var)); Sw(T 0, ofset (pos_lst lst_var s))], lst_var
  | Sval(e) -> (compile_expr lst_var e (List.length lst_var)), lst_var
  | Sbreak -> [], lst_var
  | Scontinue -> [], lst_var


let compile_fonc f = [Label(f.name); Sw(RA, ofset 0)] @ (let c_s, _ = compile_stmt ("__RA__"::(List.map (fun (Var(v)) -> v) f.args)) f.body f.name in c_s) @ (if f.name = "main" then [] else [Label("return_"^f.name); Lw(RA, ofset 0); Jr RA])
let compile_program prog ofile =
  let code = List.map compile_fonc prog.defs |> List.concat in
  let p = 
    { text =
      code@end_code;
      data = [
        ]
    }
  in
  Mips.print_program p ofile;