{
open Parser
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