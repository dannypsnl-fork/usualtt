exception InferLambda
exception NoVar
exception BadApp
exception TypeMismatched

open Term

type ctx =
  | Emp
  | Extend of var * ty * ctx

let rec infer : ctx -> term -> ty =
   fun ctx term ->
    match term with
    | Var x  -> lookup ctx x
    | App (t , u) ->
      (match infer ctx t with
       | Arrow (a, b) -> check ctx u a; b
       | _ -> raise BadApp)
    | Lam _ -> raise InferLambda
    | Let (x, a, t, u) ->
      check ctx t a;
      infer (extend ctx x a) u
    | Int _ -> Int
    | Bool _ -> Bool
and check : ctx -> term -> ty -> unit =
    fun ctx t a ->
      match (t, a) with
      | (Lam (x, t), Arrow (a, b)) -> check (extend ctx x a) t b
      | (Let (x, a, t, u), ty) ->
        check ctx t a;
        check (extend ctx x a) u ty
      | _ -> if conv (infer ctx t) a then () else raise TypeMismatched
and conv : ty -> ty -> bool =
  fun t1 t2 ->
  match (t1, t2) with
  | (TVar x1, TVar x2) -> x1 == x2
  | (Arrow (a1, b1), Arrow (a2, b2)) -> conv a1 a2 && conv b1 b2
  | (List a1, List a2) -> conv a1 a2
  | (Bool, Bool) -> true
  | (Int, Int) -> true
  | _ -> false
and extend : ctx -> var -> ty -> ctx = fun ctx x ty -> Extend (x, ty, ctx)
and lookup : ctx -> var -> ty =
  fun ctx x ->
  match ctx with
  | Extend (x', ty, ctx') -> if x == x' then ty else lookup ctx' x
  | Emp -> raise NoVar
