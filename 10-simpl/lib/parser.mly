%{
open Syntax
%}

%token ABS ARROW LPAREN RPAREN EOF IF THEN ELSE TRUE FALSE COLON BOOL_TYPE IMPLIES ZERO SUC NAT_TYPE AS
%token UNIT SEMICOLON UNIT_TYPE
%token LET IN EQ
%token <string> ID
%right IMPLIES
%start <Syntax.n_term> prog

%%

prog: e = term; EOF { e };

term:
  | e = term_seq { e }          
  | e = atomic_term; AS; t = type_expr { NApp (NAbs ("_as", t, NVar "_as"), e) } 
  | LET; x = ID; EQ; e1 = term; IN; e2 = term { NLet (x, e1, e2) }
  | ABS; x = ID; COLON; t = type_expr; ARROW; e = term { NAbs (x, t, e) }
  | IF; e1 = term; THEN; e2 = term; ELSE; e3 = term { NIf (e1, e2, e3) }
  ;

(* 应用表达式：左结合的连续应用 *)
app_term:
  | e = atomic_term { e }
  | e1 = app_term; e2 = atomic_term { NApp (e1, e2) }
  ;

(* 序列表达式：一个或多个 term，用分号连接，右结合 *)
term_seq:
  | e = app_term { e }                   (* 单个项本身不是序列 *)
  | e1 = app_term; SEMICOLON; e2 = term_seq { NApp (NAbs ("_", TUnit, e2), e1) }
  ;

(* 原子表达式：不可再分割的基础项 *)
atomic_term:
  | TRUE { NTrue }
  | FALSE { NFalse }
  | ZERO { NZero }
  | UNIT { NUnit }
  | SUC; e = atomic_term { NSuc e }
  | x = ID { NVar x }
  | LPAREN; e = term; RPAREN { e }
  ;

(* 类型表达式部分保持不变 *)
type_expr:
  | BOOL_TYPE { TBool }
  | NAT_TYPE { TNat }
  | UNIT_TYPE { TUnit }
  | t1 = type_expr; IMPLIES; t2 = type_expr { TFun (t1, t2) }
  | LPAREN; t = type_expr; RPAREN { t }
  ;