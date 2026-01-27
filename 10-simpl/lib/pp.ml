open Format
open Syntax

type precedence =
  | PAtomic
  | PUnit
  | PTrue
  | PFalse
  | PZero
  | PSuc
  | PApp
  | PAbs
  | PIF
  | PLet

(** [get_prec t] 返回项的最高优先级 *)
let get_prec (t : term) : precedence =
  match t with
  | Var _ -> PAtomic
  | App _ -> PApp
  | Abs _ -> PAbs
  | If _ -> PIF
  | True -> PTrue
  | False -> PFalse
  | Zero -> PZero
  | Suc _ -> PSuc
  | Unit -> PUnit
  | Let _ -> PLet

(** [maybe_paren current_prec outer_prec ppf f] 如果当前优先级低于外部优先级，则加上括号 *)
let maybe_paren (current : precedence) (outer : precedence) (ppf : formatter)
    (f : unit -> unit) : unit =
  if current > outer then (
    fprintf ppf "(";
    f ();
    fprintf ppf ")"
  ) else
    f ()

(** [pp_nt_term ctx outer_prec ppf t] 使用 Unicode 符号打印无名项 *)
let rec pp_nt_term (ctx : context) (outer_prec : precedence) (ppf : formatter)
    (t : term) : unit =
  let current_prec = get_prec t in
  match t with
  | True -> fprintf ppf "true"
  | False -> fprintf ppf "false"
  | Zero -> fprintf ppf "zero"
  | Suc t1 -> fprintf ppf "suc %a" (pp_nt_term ctx PAtomic) t1
  | Unit -> fprintf ppf "unit"
  | Var i ->
      let name = try fst (List.nth ctx i) with _ -> "↑" ^ string_of_int i in
      fprintf ppf "%s" name
  | Abs (x, ty, t1) ->
      maybe_paren current_prec outer_prec ppf (fun () ->
          let x' = pick_fresh_name x ctx in
          fprintf ppf "@[<hov 2>λ%s.@ %a@]" x'
            (pp_nt_term ((x', NameBind) :: ctx) PAbs)
            t1)
  | App (t1, t2) ->
      maybe_paren current_prec outer_prec ppf (fun () ->
          fprintf ppf "@[<hov 2>%a@ %a@]" (pp_nt_term ctx PApp) t1
            (pp_nt_term ctx PAtomic) t2)
  | If (t1, t2, t3) ->
      maybe_paren current_prec outer_prec ppf (fun () ->
          fprintf ppf "@[<hov 2>if %a@ then %a@ else %a@]" (pp_nt_term ctx PIF)
            t1 (pp_nt_term ctx PIF) t2 (pp_nt_term ctx PIF) t3)
  | Let (x, t1, t2) ->
      maybe_paren current_prec outer_prec ppf (fun () ->
          let x' = pick_fresh_name x ctx in
          fprintf ppf "@[<hov 2>let %s = %a in@ %a@]" x' (pp_nt_term ctx PIF) t1
            (pp_nt_term ((x', NameBind) :: ctx) PIF)
            t2)

(** 顶层接口 *)
let print_term (ctx : context) (t : term) : unit =
  pp_nt_term ctx PAbs std_formatter t;
  print_newline ()
