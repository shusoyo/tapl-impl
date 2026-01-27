{
open Parser

exception Error of string

let lexer_error msg = raise (Error msg)
}

let white = [' ' '\t' '\n']+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let int = ['0'-'9']+

rule read = 
  parse 
  | white { read lexbuf }
  
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }

  (* record *)
  | "," { COMMA }
  | "{" { LBRACE }
  | "}" { RBRACE }

  (* | "<" { LANGLE } *)
  (* | ">" { RANGLE } *)

  (* Unit and term sequence *)
  | "()" { UNIT }
  | ";" { SEMICOLON }
  | "Unit" { UNIT_TYPE }

  (* Ascription *)
  | "as" { AS }

  (* let-in binding *)
  | "=" { EQ }
  | "let" { LET }
  | "in" { IN }

  (* Abstraction and types *)
  | "." { ARROW }
  | "lambda" { ABS }
  | ":" { COLON }
  | "->" { IMPLIES }

  (* Conditional expressions *)
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }

  (* Base types *)
  | "Bool" { BOOL_TYPE }
  | "True" { TRUE }
  | "False" { FALSE }
  | "Nat" { NAT_TYPE }
  | "zero" { ZERO }
  | "suc" { SUC }

  | "(" { LPAREN }
  | ")" { RPAREN }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { lexer_error ("unkonw char: " ^ Lexing.lexeme lexbuf) }