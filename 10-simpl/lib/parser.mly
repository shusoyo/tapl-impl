%{
open Syntax
%}

%token ABS ARROW LPAREN RPAREN EOF IF THEN ELSE TRUE FALSE COLON BOOL_TYPE IMPLIES ZERO SUC NAT_TYPE
%token <string> ID

%right IMPLIES

%start <Syntax.n_term> prog

%%
prog: e = term; EOF { e };

term:
  | e = app_term { e }
  | ABS; x = ID; COLON; t = type_expr; ARROW; e = term { NAbs (x, t, e) }
  | IF; e1 = term; THEN; e2 = term; ELSE; e3 = term { NIf (e1, e2, e3) }
  ;

app_term:
  | e = atomic_term { e }
  | e1 = app_term; e2 = atomic_term { NApp (e1, e2) }
  ;

atomic_term:
  | TRUE { NTrue }
  | FALSE { NFalse }
  | ZERO { NZero }
  | SUC; e = atomic_term { NSuc e }
  | x = ID { NVar x }
  | LPAREN; e = term; RPAREN { e }
  ;

type_expr:
  | BOOL_TYPE { TBool }
  | NAT_TYPE { TNat }
  | t1 = type_expr; IMPLIES; t2 = type_expr { TFun (t1, t2) }
  | LPAREN; t = type_expr; RPAREN { t }
  ;