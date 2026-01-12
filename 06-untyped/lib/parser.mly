(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)

%{
open Syntax

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> TmApp (e, e')
  | h :: ((_ :: _) as t) -> make_apply (TmApp (e, h)) t
%}

%token <string> ID
%token ABS ARROW LPAREN RPAREN EOF

%start <Syntax.term'> prog

%%

prog:
	| e = term; EOF { e }
	;
	
term:
  | e = simpl_expr { e }
  | e = simpl_expr; es = simpl_expr+ { make_apply e es }
  | ABS; x = ID; ARROW; e = term { TmAbs (x, e) }
  ;

simpl_expr:
  | x = ID { TmVar x }
  | LPAREN; e=term; RPAREN { e } 
  ;