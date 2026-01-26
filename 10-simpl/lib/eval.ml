open Syntax

let rec typeof (ctx : context) (t : term) : ty =
  match t with
  | True -> TBool
  | False -> TBool
  | IF (t1, t2, t3) -> typeof_if ctx t1 t2 t3
  | Var i -> get_type ctx i
  | Abs (x, ty_t1, t2) -> typeof_abs ctx x ty_t1 t2
  | App (t1, t2) -> typeof_app ctx t1 t2
  | Zero -> TNat
  | Suc t1 -> typeof_suc ctx t1

and typeof_suc (ctx : context) (t1 : term) : ty =
  if typeof ctx t1 = TNat then
    TNat
  else
    failwith "Argument of successor is not a natural number"

and typeof_if (ctx : context) (t1 : term) (t2 : term) (t3 : term) : ty =
  match typeof ctx t1 with
  | TBool ->
      let ty2 = typeof ctx t2 in
      if ty2 = typeof ctx t3 then
        ty2
      else
        failwith "Branches of conditional have different types"
  | _ -> failwith "Guard of conditional not a boolean"

and typeof_abs (ctx : context) (x : string) (ty_t1 : ty) (t2 : term) : ty =
  let ctx' = add_binding ctx x (VarBind ty_t1) in
  let ty_t2 = typeof ctx' t2 in
  TFun (ty_t1, ty_t2)

and typeof_app (ctx : context) (t1 : term) (t2 : term) : ty =
  let ty_t1 = typeof ctx t1 in
  let ty_t2 = typeof ctx t2 in
  match ty_t1 with
  | TFun (ty11, ty12) ->
      if ty11 = ty_t2 then
        ty12
      else
        failwith "parameter type mismatch"
  | _ -> failwith "arrow type expected"

let typecheck (t : term) : unit = ignore (typeof empty_context t)

let rec is_val (t : term) : bool =
  match t with
  | Abs _ | True | False | Zero -> true
  | Suc x -> is_val x
  | _ -> false

let rec step (t : term) : term =
  match t with
  | App (Abs (_, ty, t), v) when is_val v -> subst_top v t
  | App (t1, t2) when is_val t1 -> App (t1, step t2)
  | App (t1, t2) -> App (step t1, t2)
  | IF (True, t2, t3) -> t2
  | IF (False, t2, t3) -> t3
  | IF (t1, t2, t3) -> IF (step t1, t2, t3)
  | _ -> t

let rec steps (t : term) : term =
  if is_val t then
    t
  else
    t |> step |> steps
