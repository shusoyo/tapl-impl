open Syntax

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
  | Abs (x, t1) ->
      Abs (x, shift d (c + 1) t1)
  | App (t1, t2) ->
      App (shift d c t1, shift d c t2)

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
  | Abs (x, t1) ->
      Abs (x, subst (j + 1) (shift 1 0 s) t1)
  | App (t1, t2) ->
      App (subst j s t1, subst j s t2)

(* E-APPABS: (λ.t₁₂) s₂  ⟶  ↑⁻¹([0 ↦ ↑¹(s₂)]t₁₂) *)
let subst_top (s : term) (t : term) : term = shift (-1) 0 (subst 0 (shift 1 0 s) t)

let is_val (t : term) : bool = match t with Abs _ -> true | _ -> false

let rec step (t : term) : term =
  match t with
  | App (Abs (_, t), v) when is_val v ->
      subst_top v t
  | App (t1, t2) when is_val t1 ->
      App (t1, step t2)
  | App (t1, t2) ->
      App (step t1, t2)
  | _ ->
      t

and steps (t : term) : term =
  if is_val t then
    t
  else
    t |> step |> steps
