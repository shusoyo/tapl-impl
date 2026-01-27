{
open Parser

exception Error of string

let lexer_error msg = raise (Error msg)
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse 
  | white { read lexbuf }
  | "()" { UNIT }
  | ";" { SEMICOLON }
  | "Unit" { UNIT_TYPE }
  | "as" { AS }
  | "=" { EQ }
  | "let" { LET }
  | "in" { IN }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { ARROW }
  | "lambda" { ABS }
  | ":" { COLON }
  | "->" { IMPLIES }
  | "Bool" { BOOL_TYPE }
  | "True" { TRUE }
  | "False" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "zero" { ZERO }
  | "suc" { SUC }
  | "Nat" { NAT_TYPE }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { lexer_error ("未知字符: " ^ Lexing.lexeme lexbuf) }