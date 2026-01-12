open Syntax

let parse (s : string) : term' =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

exception NoRuleApplies

let shifting d t =
  let rec walk c t =
    match t with
    | Abs inner -> Abs (walk (c + 1) inner)
    | App (t1, t2) -> App (walk c t1, walk c t2)
    | Var v ->
      if v >= c then
        Var (v + d)
      else
        Var v
  in
  walk 0 t
;;

(** \[j -> s] t *)
let subst j s t =
  (* c is a tag saved the number of binder enclosed *)
  let rec walk c t =
    match t with
    | Var v ->
      (* shifting by need *)
      if v = j + c then
        shifting c s
      else
        Var v
    | Abs inner -> Abs (walk (c + 1) inner)
    | App (t1, t2) -> App (walk c t1, walk c t2)
  in
  walk 0 t
;;

(** subst application, 1st [s] is increment term, 2nd [t] is renumber term *)
let subst_top s t = shifting (-1) (subst 0 (shifting 1 s) t)

let is_val t =
  match t with
  | Abs _ | Var _ -> true
  | _ -> false
;;

let rec step t =
  match t with
  | App (Abs t, v) when is_val v -> subst_top v t
  | App (t1, t2) when is_val t1 -> App (t1, step t2)
  | App (t1, t2) -> App (step t1, t2)
  | _ -> failwith "fail"
;;

let rec eval t =
  if is_val t then
    t
  else
    t |> step |> eval
;;
