type stmt = stmt_node*Lexing.position
and stmt_node =
  | Sif of expr*stmt*stmt
  | Sblock of stmt list
  | Sreturn of expr
  | Dec of left_value
  | Sassign of left_value*expr
  | Sval of expr
  | Sbreak
  | Scontinue
and left_value =
  | Var of string
and expr =
  | Const of int
  | Val of left_value
  | UnOp of expr
  | BinOp of binop * expr*expr
  | Ecall of string*expr list
and typ =
  | Int
  | Void
and binop = | Add | Sub | Mul | Div | Mod | Leq | Geq | Ge | Le | Neq | Eqq | And | Or | Nor
type def = { name : string ; args : left_value list ; body : stmt ;  return_type : typ}
and prog = { defs : def list ; }
