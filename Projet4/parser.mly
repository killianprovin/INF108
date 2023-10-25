%{
  open Ast
%}
%token <string> CST
%token <string> STR
%token <string> IDENT
%token INT VOID 
%token IF ELSE
%token RETURN BREAK CONTINUE
%token SIZEOF
%token PRINT_INT PRINT_STRING
%token LP RP
%token RB LB 
%token COMMA SEMICOLON
%token EQ
%token OR AND EQQ NEQ LE LEQ GE GEQ
%token EOF
%token PLUS MINUS TIMES DIV MOD
%token NOT ESP STAR

/* D�finitions des priorit�s et associativit�s des tokens */

%right EQ
%left OR
%left AND
%nonassoc DEQ NEQ
%nonassoc LE LEQ GE GEQ
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc uminus NOT ESP STAR

/* Point d'entr�e de la grammaire */
%start file

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.Iprogram> file

%%

file: d = def*; EOF {{ defs = d }}
;
  
def: t = typ; nom = IDENT; LP; args = separated_list(COMMA, var); RP; LB; bod = suite; RB  {{ name = nom ; args = args ; body = bod; return_type = t }}
;

var: t = typ; nom = IDENT {{ typ = t; name = nom }}
;

typ:
  | INT   { Int }
  | VOID  { Void }
;

suite: s = separated_list(SEMICOLON; stmt) { Sblock(s), $startpos }
;

stmt:
  | s = simple_stmt {s}
  | IF; e = expr; LB; s_if = suite; RB; ELSE; LB; s_else = suite; RB { Sif_else(e, s_if, s_else), $startpos }
  | IF; e = expr; LB; s_if = suite; RB { Sif(e, s_if, s_if),  $startpos }
;

simple_stmt:
  | RETURN; e = expr               { Sreturn(e) }
  | e1 = expr; eq; e2 = expe       { Sassign(e1, e2) }
  | e = expr                       { Sval(e) }
;

expr:
  | i = CST                        { Const(i) }
  | v = left_value                 { Val(v) }
  | e1 = expr o = op e2 = expr     { Op(o,e1,e2) }
  | MINUS e = expr %prec uminus    { Moins(e) } 
  | NOT e = expr                   { Not(e) }
;

left_value: v = IDENT              { Var(s) }
;


%inline op:
  | PLUS  { Add }
  | MINUS { Sub }
  | TIMES { Mul }
  | DIV   { Div }
  | MOD   { Mod }
  | LEQ   { Leq }
  | GEQ   { Geq }
  | GE    { Ge  }
  | LE    { Le  }
  | NEQ   { Neq }
  | EQQ   { Eq  }
  | AND   { And }
  | OR    { Or  } 
;