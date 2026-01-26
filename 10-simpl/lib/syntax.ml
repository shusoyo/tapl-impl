exception VariableNotFound of string

open Sexplib.Std

(** type *)
type ty = TNat | TBool | TFun of ty * ty [@@deriving sexp]

(** nameless term *)
type term =
  | Var of int
  | True
  | False
  | IF of term * term * term
  | Abs of string * ty * term
  | App of term * term
  | Zero
  | Suc of term
[@@deriving sexp]

(** named term *)
type n_term =
  | NVar of string
  | NTrue
  | NFalse
  | NIf of n_term * n_term * n_term
  | NAbs of string * ty * n_term
  | NApp of n_term * n_term
  | NZero
  | NSuc of n_term
[@@deriving sexp]

(** context *)
type binding = NameBind | VarBind of ty

type context = (string * binding) list

let empty_context : context = []

let add_binding (ctx : context) (x : string) (bind : binding) : context =
  (x, bind) :: ctx

let get_binding (ctx : context) (i : int) : string * binding = List.nth ctx i

let get_type (ctx : context) (i : int) : ty =
  match List.nth ctx i with
  | _, VarBind ty -> ty
  | x, NameBind ->
      failwith
        ("get_type_from_context: Index " ^ string_of_int i ^ ", Name " ^ x
       ^ " is not bound to a type.")

let rec index_of (ctx : context) (x : string) : int =
  match ctx with
  | [] -> raise (VariableNotFound x)
  | (h, _) :: rest ->
      if h = x then
        0
      else
        1 + index_of rest x

let rec pick_fresh_name (x : string) (ctx : context) : string =
  if List.exists (fun (y, _) -> y = x) ctx then
    pick_fresh_name (x ^ "'") ctx
  else
    x

(* CONVERSION LOGIC:
    Γ ⊢ x          ⟹  Var(i)  where index_of(x, Γ) = i
    Γ ⊢ λx. t₁     ⟹  Abs(x, t₁') 
                       where (x :: Γ) ⊢ t₁ ⟹ t₁'
    Γ ⊢ t₁ t₂      ⟹  App(t₁', t₂')
                       where Γ ⊢ t₁ ⟹ t₁' and Γ ⊢ t₂ ⟹ t₂'

    Example:
    λx. λy. x y
    Context: [y; x] (y is at index 0, x is at index 1)
    Result:  λ. λ. 1 0
  *)
let rec remove_names (ctx : context) (t : n_term) : term =
  match t with
  | NVar x -> Var (index_of ctx x)
  | NAbs (x, ty, t') -> Abs (x, ty, remove_names ((x, NameBind) :: ctx) t')
  | NApp (t1, t2) -> App (remove_names ctx t1, remove_names ctx t2)
  | NIf (t1, t2, t3) ->
      IF (remove_names ctx t1, remove_names ctx t2, remove_names ctx t3)
  | NTrue -> True
  | NFalse -> False
  | NZero -> Zero
  | NSuc t1 -> Suc (remove_names ctx t1)

(* RESTORE NAMES (Reification):
    Given a context Γ and a nameless term t, restore the variable names.

    Γ ⊢ NtVar(i)      ⟹ NVar(x)  where x = List.nth Γ i
    Γ ⊢ NtAbs(x, t₁)  ⟹ NAbs(x', t₁') 
                        where x' = pick_fresh_name(x, Γ)
                        and (x' :: Γ) ⊢ t₁ ⟹ t₁'
    Γ ⊢ NtApp(t₁, t₂) ⟹ NApp(t₁', t₂')
  *)
let rec resotre_names (ctx : context) (t : term) : n_term =
  match t with
  | Var v -> NVar (fst (List.nth ctx v))
  | Abs (x, ty, t) ->
      let x' = pick_fresh_name x ctx in
      NAbs (x', ty, resotre_names ((x', NameBind) :: ctx) t)
  | App (t1, t2) -> NApp (resotre_names ctx t1, resotre_names ctx t2)
  | IF (t1, t2, t3) ->
      NIf (resotre_names ctx t1, resotre_names ctx t2, resotre_names ctx t3)
  | True -> NTrue
  | False -> NFalse
  | Zero -> NZero
  | Suc t1 -> NSuc (resotre_names ctx t1)

(* 6.2.1 DEFINITION [SHIFTING]: 
   The d-place shift of a term t above cutoff c, written ↑ᵈ꜀(t), 
   is defined as follows:

      ↑ᵈ꜀(k)         =  k          if k < c
                        k + d      if k >= c

      ↑ᵈ꜀(λ.t1)      =  λ. ↑ᵈ꜀₊₁(t1)

      ↑ᵈ꜀(t1 t2)     =  ↑ᵈ꜀(t1) ↑ᵈ꜀(t2)

   We write ↑ᵈ(t) for ↑ᵈ₀(t).
*)
let rec shift (d : int) (c : int) (t : term) : term =
  match t with
  | Var k ->
      if k >= c then
        Var (k + d)
      else
        Var k
  | Abs (x, ty, t1) -> Abs (x, ty, shift d (c + 1) t1)
  | App (t1, t2) -> App (shift d c t1, shift d c t2)
  | IF (t1, t2, t3) -> IF (shift d c t1, shift d c t2, shift d c t3)
  | _ -> t

(* 6.2.4 DEFINITION [SUBSTITUTION]:
   The substitution of a term s for variable number j in a term t,
   written [j ↦ s]t, is defined as follows:

      [j ↦ s]k       =  s              if k = j
                        k              otherwise

      [j ↦ s](λ.t1)  =  λ. [j+1 ↦ ↑¹(s)]t1

      [j ↦ s](t1 t2) =  ([j ↦ s]t1 [j ↦ s]t2)
*)
let rec subst (j : int) (s : term) (t : term) : term =
  match t with
  | Var k ->
      if k = j then
        s
      else
        Var k
  | Abs (x, ty, t1) -> Abs (x, ty, subst (j + 1) (shift 1 0 s) t1)
  | App (t1, t2) -> App (subst j s t1, subst j s t2)
  | IF (t1, t2, t3) -> IF (subst j s t1, subst j s t2, subst j s t3)
  | Suc t1 -> Suc (subst j s t1)
  | _ -> t

(* E-APPABS: (λ.t₁₂) s₂  ⟶  ↑⁻¹([0 ↦ ↑¹(s₂)]t₁₂) *)
let subst_top (s : term) (t : term) : term =
  shift (-1) 0 (subst 0 (shift 1 0 s) t)
