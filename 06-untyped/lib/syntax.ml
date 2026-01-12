(** ast *)
type term =
  | Var of int (** use de bruijn index *)
  | Abs of term (** abstraction: ^ x => t *)
  | App of term * term

type term' =
  | TmVar of string
  | TmAbs of string * term'
  | TmApp of term' * term'
