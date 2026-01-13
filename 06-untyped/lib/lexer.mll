{
open Parser

exception Error of string

let lexer_error msg = raise (Error msg)
}

let white = [' ' '\t']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

rule read = 
  parse 
  | white { read lexbuf }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { ARROW }
  | "lambda" { ABS }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { lexer_error ("未知字符: " ^ Lexing.lexeme lexbuf) }