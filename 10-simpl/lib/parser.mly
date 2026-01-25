(* This file uses some advanced parsing techniques
   to parse juxtaposed applications [e1 e2 e3] the
	 same way as OCaml does. *)

%{
open Syntax

(** [make_apply e [e1; e2; ...]] makes the application  
    [e e1 e2 ...]).  Requires: the list argument is non-empty. *)
let rec make_apply e = function
  | [] -> failwith "precondition violated"
  | [e'] -> NApp (e, e')
  | h :: ((_ :: _) as t) -> make_apply (NApp (e, h)) t
%}

%token <string> ID
%token ABS ARROW LPAREN RPAREN EOF IF THEN ELSE TRUE FALSE COLON BOOL_TYPE IMPLIES

%start <Syntax.n_term> prog

%%

prog:
	| e = term; EOF { e }
	;
	
term:
  | e = simpl_expr { e }
  | e = simpl_expr; es = simpl_expr+ { make_apply e es }
  | TRUE { NTrue }
  | FALSE { NFalse }
  | ABS; x = ID; COLON; t = type_expr; ARROW; e = term { NAbs (x, t, e) }
  | IF; e1 = term; THEN; e2 = term; ELSE; e3 = term { NIf (e1, e2, e3) }
  ;

type_expr:
  | BOOL_TYPE { TBool }
  | t1 = type_expr; IMPLIES; t2 = type_expr { TFun (t1, t2) }
  | LPAREN; t = type_expr; RPAREN { t }
  ;

simpl_expr:
  | x = ID { NVar x }
  | LPAREN; e=term; RPAREN { e } 
  ;