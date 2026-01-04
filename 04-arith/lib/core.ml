open Syntax

exception NoRuleApplies

let rec isnumericval t =
  match t with
  | TmZero -> true
  | TmSucc t1 -> isnumericval t1
  | _ -> false
;;

let rec isval t =
  match t with
  | TmTrue | TmFalse -> true
  | t when isnumericval t -> true
  | _ -> false
;;

let rec step t =
  match t with
  | TmIf (TmTrue, t2, _) -> t2
  | TmIf (TmFalse, _, t3) -> t3
  | TmIf (t1, t2, t3) ->
    let t1' = step t1 in
    TmIf (t1', t2, t3)
  | TmSucc t1 ->
    let t1' = step t1 in
    TmSucc t1'
  | TmPred TmZero -> TmZero
  | TmPred (TmSucc nv1) when isnumericval nv1 -> nv1
  | TmPred t1 ->
    let t1' = step t1 in
    TmPred t1'
  | TmIsZero TmZero -> TmTrue
  | TmIsZero (TmSucc nv1) when isnumericval nv1 -> TmFalse
  | TmIsZero t1 ->
    let t1' = step t1 in
    TmIsZero t1'
  | _ -> raise NoRuleApplies
;;

let rec eval t =
  if isval t then
    t
  else
    t |> step |> eval
;;

let rec bigstep t =
  match t with
  | x when isval x -> x
  | TmIf (e1, e2, e3) -> bigstep_if e1 e2 e3
  | TmSucc e -> bigstep_succ e
  | TmPred e -> bigstep_pred e
  | TmIsZero e -> bigstep_iszero e
  | _ -> raise NoRuleApplies

and bigstep_if e1 e2 e3 =
  match bigstep e1, e2, e3 with
  | TmTrue, _, _ -> bigstep e2
  | TmFalse, _, _ -> bigstep e3
  | _ -> failwith "if"

and bigstep_succ e =
  match bigstep e with
  | v when isnumericval v -> v
  | _ -> failwith "succ"

and bigstep_pred e =
  match bigstep e with
  | TmZero -> TmZero
  | TmSucc v when isnumericval v -> v
  | _ -> failwith "pred"

and bigstep_iszero e =
  match bigstep e with
  | TmZero -> TmTrue
  | TmSucc v when isnumericval v -> TmFalse
  | _ -> failwith "iszero"
;;
