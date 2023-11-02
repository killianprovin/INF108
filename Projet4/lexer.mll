
(* Analyseur lexical *)

{
  open Lexing
  open Parser
   
  exception Lexing_error of char
    
  let kwd_tbl = ["int", INT; "void", VOID; "if", IF; "else", ELSE; "return", RETURN; "break", BREAK; "continue", CONTINUE]
  let id_or_kwd s = try List.assoc s kwd_tbl with _ -> IDENT s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse
  | '\n'    { newline lexbuf; token lexbuf }
  | space+  { token lexbuf }
  | ident as id { id_or_kwd id }
  | "=="    { EQQ }
  | "!="    { NEQ }
  | "<="    { LEQ }
  | ">="    { GEQ }
  | '<'     { LE }
  | '>'     { GE }
  | "||"     { OR }
  | "&&"     { AND }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '%'     { MOD }
  | '='     { EQ }
  | '('     { LP }
  | ')'     { RP }
  | '{'     { LB }
  | '}'     { RB }
  | ';'     { SEMICOLON }
  | ','     { COMMA }
  | '!'     { NOT }
  | integer as s { CST (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error c) }
 

