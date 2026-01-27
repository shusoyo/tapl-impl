%{
open Syntax
%}

%token ABS ARROW LPAREN RPAREN EOF IF THEN ELSE TRUE FALSE COLON BOOL_TYPE IMPLIES ZERO SUC NAT_TYPE AS
%token UNIT SEMICOLON UNIT_TYPE

%token COMMA LBRACE RBRACE

%token LET IN EQ

%token <string> ID
%token <int> INT

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
  
app_term:
  | e = atomic_term { e }
  | e1 = app_term; e2 = atomic_term { NApp (e1, e2) }
  ;

term_seq:
  | e = app_term { e }                   
  | e1 = app_term; SEMICOLON; e2 = term_seq { NApp (NAbs ("_", TUnit, e2), e1) }
  ;

record_atom_term:
  | x = ID; EQ; e = atomic_term; { (x, e) }
  ;

record_term:
  (* record_term *)
  | LBRACE; ts = separated_list(COMMA, record_atom_term); RBRACE { NRecord (ts) }
  (* tuple *)
  | LBRACE; ts = separated_nonempty_list(COMMA, atomic_term); RBRACE  { 
      let fields = List.mapi (fun i t -> ("_" ^ string_of_int i, t)) ts in
      NRecord fields 
    }
  | t = record_term; ARROW; x = ID  { NProj (t, x)  }
  | t = record_term; ARROW; x = INT { NProj (t, "_" ^ string_of_int x)  }


atomic_term:
  | TRUE { NTrue }
  | FALSE { NFalse }
  | ZERO { NZero }
  | UNIT { NUnit }
  | SUC; e = atomic_term { NSuc e }
  | e = record_term { e }
  | x = ID { NVar x }
  | LPAREN; e = term; RPAREN { e }
  ;


record_type_term:
  | x = ID; COLON; e = type_expr; { (x, e) }
  ;

type_expr:
  | BOOL_TYPE { TBool }
  | NAT_TYPE { TNat }
  | UNIT_TYPE { TUnit }
  | t1 = type_expr; IMPLIES; t2 = type_expr { TFun (t1, t2) }

  | LBRACE; ts = separated_list(COMMA, record_type_term); RBRACE { TRecord (ts) }
  | LBRACE; ts = separated_nonempty_list(COMMA, type_expr); RBRACE  { 
      let fields = List.mapi (fun i t -> ("_" ^ string_of_int i, t)) ts in
      TRecord fields 
    }

  | LPAREN; t = type_expr; RPAREN { t }
  ;