(** nameless term *)
type term = Var of int | Abs of string * term | App of term * term

(** named term *)
type n_term = NVar of string | NAbs of string * n_term | NApp of n_term * n_term

exception VariableNotFound of string

type context = string list

let empty_context : context = []

let rec index_of (ctx : context) (x : string) : int =
  match ctx with
  | [] ->
      raise (VariableNotFound x)
  | h :: rest ->
      if h = x then
        0
      else
        1 + index_of rest x

let rec pick_fresh_name (x : string) (ctx : context) : string =
  if List.mem x ctx then
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
  | NVar x ->
      Var (index_of ctx x)
  | NAbs (x, t') ->
      Abs (x, remove_names (x :: ctx) t')
  | NApp (t1, t2) ->
      App (remove_names ctx t1, remove_names ctx t2)

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
  | Var v ->
      NVar (List.nth ctx v)
  | Abs (x, t) ->
      let x' = pick_fresh_name x ctx in
      NAbs (x', resotre_names (x' :: ctx) t)
  | App (t1, t2) ->
      NApp (resotre_names ctx t1, resotre_names ctx t2)
