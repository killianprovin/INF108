%{
  open Ast
%}
%token <int> CST
%token <string> IDENT
%token INT VOID 
%token IF ELSE
%token RETURN BREAK CONTINUE
%token LP RP
%token RB LB 
%token COMMA SEMICOLON
%token EQ
%token OR AND EQQ NEQ LE LEQ GE GEQ
%token EOF
%token PLUS MINUS TIMES DIV MOD
%token NOT

/* D�finitions des priorit�s et associativit�s des tokens */

%right EQ
%left OR
%left AND
%nonassoc EQQ NEQ
%nonassoc LE LEQ GE GEQ
%left PLUS MINUS 
%left TIMES DIV MOD
%nonassoc uminus NOT

/* Point d'entr�e de la grammaire */
%start file

/* Type des valeurs retourn�es par l'analyseur syntaxique */
%type <Ast.prog> file

%%

file: d = def*; EOF {{ defs = d }}
;
  
def: t = typ; nom = IDENT; LP; args = separated_list(COMMA, var); RP; LB; bod = suite; RB  {{ name = nom ; args = args ; body = bod; return_type = t }}
;

var: t = typ; l = left_value { l }
;

typ:
  | INT   { Int }
  | VOID  { Void }
;

suite: s = stmt* { Sblock(s), $startpos }
;

stmt:
  | s = simple_stmt; SEMICOLON {s}
  | IF; LP; e = expr; RP; LB; s_if = suite; RB; ELSE; LB; s_else = suite; RB { Sif(e, s_if, s_else), $startpos }
  | IF; LP; e = expr; RP; LB; s_if = suite; RB { Sif(e, s_if, (Sblock([]),$startpos)),  $startpos }
  | IF; LP; e = expr; RP; s = simple_stmt; SEMICOLON {Sif(e, s, (Sblock([]),$startpos)),  $startpos}
;

simple_stmt:
  | CONTINUE                        { Sbreak, $startpos }
  | BREAK                           { Scontinue, $startpos }
  | RETURN; e = expr                { Sreturn(e), $startpos }
  | v = var                         { Dec(v), $startpos } (*Declaration de la variable*)
  | v = IDENT; EQ; e1 = expr        { Sassign(Var(v), e1), $startpos } (*assignation de la variable*)
  | v = var; EQ; e1 = expr          { Sblock([(Dec(v), $startpos); (Sassign(v, e1), $startpos)]), $startpos } (* les deux en meme temps *)
  | e = expr                        { Sval(e), $startpos }
;

expr:
  | i = CST                        { Const i }
  | v = left_value                 { Val(v) }
  | e1 = expr o = op e2 = expr     { BinOp(o,e1,e2) }
  | MINUS e = expr %prec uminus    { BinOp(Sub, Const 0, e) } 
  | NOT e = expr                                            { UnOp(e) }
  | nom = IDENT; LP; args = separated_list(COMMA, expr); RP  { Ecall(nom, args) }
  | LP e = expr RP                 { e }
;

left_value: v = IDENT              { Var(v) }
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
  | EQQ   { Eqq  }
  | AND   { And }
  | OR    { Or  } 
  ;