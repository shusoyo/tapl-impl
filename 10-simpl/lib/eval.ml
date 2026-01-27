open Syntax

let rec typeof (ctx : context) (t : term) : ty =
  match t with
  (* Condition *)
  | True -> TBool
  | False -> TBool
  | If (t1, t2, t3) -> typeof_if ctx t1 t2 t3
  (* Unit *)
  | Unit -> TUnit
  (* base lambda-calculus *)
  | Var i -> get_type ctx i
  | Abs (x, ty_t1, t2) -> typeof_abs ctx x ty_t1 t2
  | App (t1, t2) -> typeof_app ctx t1 t2
  (* Natural number *)
  | Zero -> TNat
  | Suc t1 -> typeof_suc ctx t1
  (* let binding *)
  | Let (x, t1, t2) -> typeof_let ctx x t1 t2
  (* record *)
  | Record fields -> typeof_record ctx fields
  | Proj (t1, label) -> typeof_proj ctx t1 label

and typeof_record (ctx : context) (fields : (string * term) list) : ty =
  TRecord (List.map (fun (label, ti) -> (label, typeof ctx ti)) fields)

and typeof_proj (ctx : context) (t1 : term) (label : string) : ty =
  match typeof ctx t1 with
  | TRecord ts -> (
      match List.find_opt (fun (x, _) -> x = label) ts with
      | Some (_, ty) -> ty
      | None -> failwith ("Label " ^ label ^ " not found in record type"))
  | _ -> failwith ("Label " ^ label ^ " not found in record type")

and typeof_let (ctx : context) (x : string) (t1 : term) (t2 : term) : ty =
  let ty_t1 = typeof ctx t1 in
  let ctx' = add_binding ctx x (VarBind ty_t1) in
  typeof ctx' t2

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

(* Eval *)
let rec is_numerical (t : term) : bool =
  match t with Zero -> true | Suc t1 -> is_numerical t1 | _ -> false

let rec is_val (t : term) : bool =
  match t with
  | Abs _ | True | False | Zero -> true
  | Suc x -> is_numerical x
  | Record fields -> List.for_all (fun (_, ti) -> is_val ti) fields
  | _ -> false

let rec step (t : term) : term =
  match t with
  | App (t1, t2) -> step_app t1 t2
  | If (t1, t2, t3) -> step_if t1 t2 t3
  | Let (x, t1, t2) -> step_let x t1 t2
  | Record fields -> step_record fields
  | Proj (t1, label) -> step_proj t1 label
  | _ -> t

and step_proj (t1 : term) (label : string) : term =
  match t1 with
  | Record fields when List.for_all (fun (_, ti) -> is_val ti) fields -> (
      match List.assoc_opt label fields with
      | Some ti -> ti
      | None -> failwith ("Label " ^ label ^ " not found in record"))
  | _ -> Proj (step t1, label)

and step_record (fields : (string * term) list) : term =
  let step_one (label, t) =
    if is_val t then
      (label, t)
    else
      (label, steps t)
  in
  Record (List.map step_one fields)

and step_let (x : string) (t1 : term) (t2 : term) : term =
  if is_val t1 then
    subst_top t1 t2
  else
    Let (x, step t1, t2)

and step_app (t1 : term) (t2 : term) : term =
  match t1 with
  | Abs (_, ty, t) when is_val t2 -> subst_top t2 t
  | t1 when is_val t1 -> App (t1, step t2)
  | t1 -> App (step t1, t2)

and step_if (t1 : term) (t2 : term) (t3 : term) : term =
  match t1 with True -> t2 | False -> t3 | _ -> If (step t1, t2, t3)

and steps (t : term) : term =
  if is_val t then
    t
  else
    t |> step |> steps
