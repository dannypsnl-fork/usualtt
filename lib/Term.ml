
type var = String.t

type ty = 
  (* primitive *)
  | Bool
  | Int
  (* list a *)
  | List of ty
  (* a -> b *)
  | Arrow of ty * ty
  (* type variable *)
  | TVar of var

type tm =
  (* x *)
  | Var of  var
  (* t u *)
  | App of tm * tm
  (* λ x . t *)
  | Lam of var * tm
  (* let x : a = t; u *)
  | Let of var * ty * tm * tm
