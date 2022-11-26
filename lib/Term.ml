
type var = String.t

type ty = 
  | Bool
  | Int
  | List of ty
  | Arrow of ty * ty

type tm =
  (* x *)
  | Var of  var
  (* t u *)
  | App of tm * tm
  (* λ x . t *)
  | Lam of var * tm
  (* let x : a = t; u *)
  | Let of var * ty * tm * tm
