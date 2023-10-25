open Ast ;;

exception Error of string ;;
exception RuntimeError of string * Lexing.position ;;
type values = 
| VInt of int 
| VBool of bool
| VString of string
| VList of values list
| VNone

exception Return of values;;
exception OpperationInvalide of string*string*string
exception VariableInexistante of string
exception FonctionInexistante of string
exception TypeInvalide

(*fonction range*)
let rec range start stop step = if (stop - start) > 0 then (VInt(start)::(range (start + step) stop step)) else []

(*creation des fonctions pour afficher la position des erreur*)
let create_store () =
  let data = ref None in
  let get () = match !data with | Some x -> x | None -> failwith "Position Invalide" in
  let set x = data := Some x in
  (get,set)
let get_pos, set_pos = create_store ()

let rec compare_lst l1 l2 = match (l1, l2) with
  | ([], []) -> false
  | ([], _::_) -> true
  | (_::_, []) -> false
  | (e1::q1, e2::q2) when e1 = e2 -> compare_lst q1 q2
  | (e1::_, e2::_) -> e1 < e2


(*transforme un string en list de code askii de ses caractere*)
  

let str_to_listint s = 
  let rec exp i l =
    if i < 0 then l else exp (i - 1) ((Char.code s.[i]):: l) 
  in
  exp (String.length s - 1) []

  let str_to_vlist s = 
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (VString(Char.escaped s.[i]):: l) 
    in
    exp (String.length s - 1) []

(*Fonction qui gere les erreurs*)
let erreur txt= raise (RuntimeError(txt, get_pos()))


(*fonction permetant l'afichage de toutes les values en les transformant en String*)
let rec values_to_string v = match v with 
  | VInt(i)    -> string_of_int i
  | VBool(b)   -> string_of_bool b
  | VString(s) -> s
  | VList(lst) -> let str = (List.fold_left (fun acc vi -> acc ^ (values_to_string vi) ^ ", ") "" lst) in
                  "[" ^ (String.sub str 0 ((String.length str) - 2)) ^ "]"
  | VNone -> "None"


(*fonction permetant l'afichage du type toutes les values*)
let rec vtype v = match v with
  | VInt(_) -> "Int"
  | VBool(_) -> "Bool"
  | VString(_) -> "String"
  | VList(lst) -> "List" ^ (match lst with
                              | [] -> ""
                              | e::q -> if (List.for_all (fun e1 -> vtype(e1) = vtype(e)) q) then " " ^ vtype(e) else " types mixtes")
  | VNone -> "None"

(*Fonction permetant d'utiliser les List, String, ... comme des booleen*)
let eval_val v = match v with
  | VBool(b) -> b
  | VString(s) -> (String.length s) > 0
  | VList(l) -> (List.length l) > 0
  | VInt(i) -> i != 0
  | VNone -> false



let eval program_ast out =

  let fonctions = Hashtbl.create 0 in
  let variable_glob = Hashtbl.create 0 in


  let main_fct = List.hd (program_ast.defs) in
  List.iter (fun def_fun -> Hashtbl.add fonctions def_fun.name (def_fun.body, def_fun.args); Hashtbl.add variable_glob def_fun.name VNone) (List.tl (program_ast.defs));


  let rec eval_expr (expr:Ast.expr) vars_loc = match expr with

    | Op(bop, e1, e2) -> (match bop with
                        | And -> (match (eval_expr e1 vars_loc) with | VBool(false) -> VBool(false) | VBool(true) -> (match (eval_expr e1 vars_loc) with | VBool(b) -> VBool(b) | _ -> raise TypeInvalide) | _ -> raise TypeInvalide)
                        | Or -> (match (eval_expr e1 vars_loc) with | VBool(true) -> VBool(true) | VBool(false) -> (match (eval_expr e1 vars_loc) with | VBool(b) -> VBool(b) | _ -> raise TypeInvalide) | _ -> raise TypeInvalide)
                        | _ ->
                        (match (bop, (eval_expr e1 vars_loc), (eval_expr e2 vars_loc)) with
                          | (Add, VInt(i1), VInt(i2))                                     -> VInt(i1 + i2)
                          | (Sub, VInt(i1), VInt(i2))                                     -> VInt(i1 - i2)
                          | (Div, VInt(i1), VInt(i2))                                     -> VInt(i1 / i2)
                          | (Mod, VInt(i1), VInt(i2))                                     -> VInt(i1 mod i2)
                          | (Add, VString(s1), VString(s2))                               -> VString(s1 ^ s2)
                          | (Add, VBool(b1), VBool(b2)) | (Or, VBool(b1), VBool(b2))      -> VBool(b1 || b2)
                          | (Add, VList(l1), VList(l2))                                   -> VList(List.append l1 l2)
                          | (Mul, VInt(i1), VInt(i2))                                     -> VInt(i1 * i2)
                          | (Mul, VString(s1), VInt(i2))                                  -> VString(List.fold_left (fun acc e -> acc ^ e) "" (List.init i2 (fun _ -> s1)))
                          | (Mul, VBool(b1), VBool(b2)) | (And, VBool(b1), VBool(b2))     -> VBool(b1 && b2)
                          | (Eq, v1, v2)                                                  -> VBool(v1 = v2)
                          | (Neq, v1, v2)                                                 -> VBool(v1 <> v2)
                          | (Le, VInt(i1), VInt(i2))                                      -> VBool(i1 < i2)
                          | (Leq, VInt(i1), VInt(i2))                                     -> VBool(i1 <= i2)
                          | (Ge, VInt(i1), VInt(i2))                                      -> VBool(i1 > i2)
                          | (Geq, VInt(i1), VInt(i2))                                     -> VBool(i1 >= i2)
                          | (Le, VList(l1), VList(l2))                                    -> VBool(compare_lst l1 l2)
                          | (Leq, VList(l1), VList(l2))                                    -> VBool(compare_lst l1 l2 || l1 = l2)
                          | (Ge, VList(l1), VList(l2))                                    -> VBool(not(compare_lst l1 l2) && not(l1 = l2))
                          | (Geq, VList(l1), VList(l2))                                    -> VBool(not(compare_lst l1 l2))
                          | (Le, VString(s1), VString(s2))                                    -> VBool(compare_lst (str_to_listint s1) (str_to_listint s2))
                          | (Leq, VString(s1), VString(s2))                                    -> VBool(compare_lst (str_to_listint s1) (str_to_listint s2) || (str_to_listint s1) = (str_to_listint s2))
                          | (Ge, VString(s1), VString(s2))                                    -> VBool(not(compare_lst (str_to_listint s1) (str_to_listint s2)) && not((str_to_listint s1) = (str_to_listint s2)))
                          | (Geq, VString(s1), VString(s2))                                    -> VBool(not(compare_lst (str_to_listint s1) (str_to_listint s2)))
                          | _ -> raise (OpperationInvalide(vtype((eval_expr e1 vars_loc)), str_op(bop), vtype((eval_expr e2 vars_loc))))))

    | Ecall(fct,args) ->
       call fct (List.map (fun e -> eval_expr e vars_loc) args)

    | Const(c) -> (match c with 
      | Int(i) -> VInt(int_of_string i)
      | Str(s) -> VString(s)
      | Bool(b) -> VBool(b)
      | Non -> VNone)

    | List(l) -> VList(List.map (fun e -> eval_expr e vars_loc) l)

    | Val l -> (match l with
                   (*On essaye de trouver la valeur dans les variables local d'abord puis global*)
                  | Var nom -> (try(Hashtbl.find vars_loc nom) with Not_found -> try (Hashtbl.find variable_glob nom) with Not_found -> raise (VariableInexistante(nom)))

                  | Tab(e1, e2) -> (match (eval_expr e1 vars_loc, eval_expr e2 vars_loc) with
                                    | (VList(l), VInt(i)) -> List.nth l i
                                    | (VString(s), VInt(i)) -> VString(Char.escaped s.[i])
                                    | _ -> raise (TypeInvalide)
                ))
    | Moins e -> (match (eval_expr e vars_loc) with | VInt(i) -> VInt(-i) | _ -> raise (TypeInvalide))
    | Not e -> (match (eval_expr e vars_loc) with | VBool(b) -> VBool(not b) | _ -> raise (TypeInvalide))
    | ListCompr(e1, s, e2) -> match (eval_expr e2 vars_loc) with
                                | VList(l) -> VList(List.map (fun v -> Hashtbl.replace vars_loc s v; eval_expr e1 vars_loc) l)
                                | VString(st) -> VList(List.map (fun v -> Hashtbl.replace vars_loc s v; eval_expr e1 vars_loc) (str_to_vlist st))
                                | _ -> raise (TypeInvalide)

  and eval_stmt (stmt_node,_pos) vars_loc =
    set_pos _pos;
    match stmt_node with
      | Sval e -> eval_expr e vars_loc

      | Sblock b -> List.fold_left (fun _ e -> eval_stmt e vars_loc) VNone b  

      | Sassign(l,e) -> (match l with 
                        | Var nom -> Hashtbl.add vars_loc nom (eval_expr e vars_loc)

                        | Tab(_, _) -> (
                          (*fonction qui recupere la liste a modifier et les indices de modifcation*)
                          let rec indices_modification lv1 = (match lv1 with
                            | Var(nom) -> (nom, [])
                            | Tab(Val(lv2), e2) ->  let tab, lst_i = indices_modification lv2 in (tab, (eval_expr e2 vars_loc)::lst_i)
                            | _ -> raise (TypeInvalide))

                          in 
                          (*fonction qui renvoie la liste modifier*)
                          let rec modifie_tab lst lst_i = (match (lst, lst_i) with
                            | (VList(vl), [VInt(i)]) -> (let a = Array.of_list vl in a.(i) <- (eval_expr e vars_loc); VList(Array.to_list a))
                            | (VList(vl), (VInt(i))::q) -> (let a = Array.of_list vl in a.(i) <- (modifie_tab (List.nth vl i) q); VList(Array.to_list a))
                            | (_, []) -> (eval_expr e vars_loc)
                            | _ -> raise (TypeInvalide))

                          in let nom, lst_i = indices_modification l in Hashtbl.replace vars_loc nom (modifie_tab (Hashtbl.find vars_loc nom) (List.rev lst_i)))); VNone

      | Sfor(nom, e, st) -> (match (eval_expr e vars_loc) with 
                              | VList(l) -> List.fold_left (fun _ v -> Hashtbl.replace vars_loc nom v; eval_stmt st vars_loc) VNone l
                              | VString(s) -> String.fold_left (fun _ v -> Hashtbl.replace vars_loc nom (VString(Char.escaped v)); eval_stmt st vars_loc) VNone s
                              | _ -> raise (TypeInvalide))

      | Sreturn e -> let value = (eval_expr e vars_loc) in raise (Return(value)); 
      | Swhile(e, st) -> while (eval_val (eval_expr e vars_loc)) do (let _ = eval_stmt st vars_loc in ()) done; VNone
      | Sif(e, st) -> if (eval_val (eval_expr e vars_loc)) then (eval_stmt st vars_loc) else VNone
      | Sifelse(e, st_if, st_else) -> if (eval_val (eval_expr e vars_loc)) then (eval_stmt st_if vars_loc) else (eval_stmt st_else vars_loc)


  and call (fct:string) (args:values list) =
    assert(main_fct.name = "-#-main-#-");
    try (
      let body, nom_args = Hashtbl.find fonctions fct in
      let variables_loc = Hashtbl.create (List.length nom_args) in
      List.iter2 (fun nom v -> Hashtbl.add variables_loc nom v) nom_args args;
      try (eval_stmt body variables_loc)
      with 
        | Return value -> value)
    with
      | Not_found -> match fct with
        | "print" -> print_string (values_to_string (List.hd args)); print_string "\n"; out (values_to_string (List.hd args)); VNone
        | "type" -> VString(vtype (List.hd args))
        | "len" ->( match (List.hd args) with 
                      | VString(s) -> VInt(String.length s)
                      | VList(lst) -> VInt(List.length lst)
                      | _ -> raise (TypeInvalide))
        | "range" ->  (match args with
                        | [VInt(i)] -> VList(range 0 i 1) 
                        | [VInt(i1); VInt(i2)] when i2 >= i1 -> VList(range i1 i2 1) 
                        | [VInt(i1); VInt(i2); VInt(i3)] when i2 >= i1 && i3 > 0-> VList(range i1 i2 i3) 
                        | _ -> raise (TypeInvalide))
        | "-#-main-#-" -> eval_stmt main_fct.body variable_glob
        | _ ->  raise (FonctionInexistante(fct))

  in

  let run (_a:Ast.prog) =
    let _ = call "-#-main-#-" [] in ()
  in
  try (run program_ast)
  with
    | OpperationInvalide(v1, o, v2) -> erreur ("Impossible d'effectuer l'opperation " ^ o ^ " avec les types " ^ v1 ^ " et " ^ v2)
    | VariableInexistante(v) -> erreur ("Variable \"" ^ v ^ "\" inexistante")
    | FonctionInexistante(f) -> erreur ("Fonction \"" ^ f ^ "\" inexistante")
    | TypeInvalide -> erreur ("Type invalide")
    | Invalid_argument(s) -> erreur (s)
    | Failure(s) -> erreur (s)