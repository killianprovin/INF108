type stmt = stmt_node*Lexing.position
and stmt_node =
  | Sif of expr*stmt*stmt
  | Sblock of stmt list
  | Sreturn of expr
  | Sassign of expr*expr
  | Sval of expr
and left_value =
  | Var of string
and expr =
  | Const of int
  | Val of left_value
  | UnOp of expr
  | BinOp of binop * expr*expr
  | Ecall of string*expr list

type Iprogram = (string*iAST) list * (string*int) list
and iAST =
  | Iif of expr*iAST*iAST | Iblock of iAST list
  | Ireturn of expr | Iassign of left_value*expr
  | Ival of expr
and value =
  | Ileft of left_value | Iconst of int
and left_value = pos * int (* position in memory and size *)
and pos =
  | Ilocal of int (* offset to FP *)
  | Iglobal of string (* label *)
  | Ideref of expr (* for pointers *)
and expr =
  | Iunop of value | Ibinop of binop * value * value
  | Icall of pos * string * int (* label + offset *)