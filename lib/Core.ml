open Term

exception InferLambda of term
exception BadApp
exception BadType
exception BadLift
exception NoVar of var
exception TypeMismatch of ty * ty

type ctx =
  | Emp
  | Extend of var * ty_val * ctx
type env = ctx

let rec infer : env -> ctx -> term -> ty_val =
  fun env ctx term ->
  match term with
  | Var x -> 
    begin
      match lookup ctx x with
      | Some t -> t
      | None -> raise (NoVar x)
    end
  | App (t , u) ->
    begin
      match infer env ctx t with
      | TArrow (a, b) ->
        check env ctx u a;
        b
      | _ -> raise BadApp
    end
  | Lam _ -> raise (InferLambda term)
  | Let (x, a, t, u) ->
    let a' = (eval env (top_lift a))
    in check env ctx t a';
    infer env (extend ctx x a') u
  | Int _ -> eval env Int
  | Bool _ -> eval env Bool
and check : env -> ctx -> term -> ty_val -> unit =
  fun env ctx t a ->
  match (t, a) with
  | (Lam (x, t), TArrow (a, b)) -> check env (extend ctx x a) t b
  | (Let (x, a, t, u), ty) ->
    let a' = (eval env a)
    in check env ctx t a';
    check env (extend ctx x a') u ty
  | _ -> let ty = infer env ctx t
         in if conv env ty a then () else raise (TypeMismatch (quote env ty, quote env a))
and eval : env -> ty -> ty_val =
  fun env ty ->
  match ty with
  | List a -> TApp ((TVar "List"), (eval env a))
  | Arrow (a, b) -> TArrow ((eval env a), (eval env b))
  | Bool -> TVar "Bool"
  | Int -> TVar "Int"
  | TSchema (v, ty) -> TLam (v, (fun u -> eval (extend env v u) ty))
  | TVar x -> TVar x
and top_lift : ty -> ty = fun ty -> lift (to_lift ty) ty
and lift : var list -> ty -> ty =
  fun vs ty ->
  match vs with
  | [] -> ty
  | v :: vs -> TSchema (v, (lift vs ty))
and to_lift : ty -> var list =
  fun ty ->
  match ty with
  | Bool -> []
  | Int -> []
  | List a -> to_lift a
  | Arrow (a, b) -> List.append (to_lift a) (to_lift b)
  | TVar x -> [x]
  | _ -> raise BadLift
and conv : env -> ty_val -> ty_val -> bool =
  fun env t1 t2 ->
  match (t1, t2) with
  | (TVar x1, TVar x2) -> String.equal x1 x2
  | (TLam (x, t), TLam (_, t')) -> conv (extend env x (TVar x)) (t (TVar x)) (t' (TVar x))
  | (TLam (x, t), u) ->  conv (extend env x (TVar x)) (t (TVar x)) (TApp (u, (TVar x)))
  | (u, TLam (x, t)) ->  conv (extend env x (TVar x)) (TApp (u, (TVar x))) (t (TVar x))
  | (TArrow (a1, b1), TArrow (a2, b2)) -> conv env a1 a2 && conv env b1 b2
  | _ -> false
and extend : ctx -> var -> ty_val -> ctx = fun ctx x ty -> Extend (x, ty, ctx)
and lookup : ctx -> var -> ty_val option =
  fun ctx x ->
  match ctx with
  | Extend (x', ty, ctx') -> if String.equal x x' then Some ty else lookup ctx' x
  | Emp -> None
and quote : env -> ty_val -> ty =
  fun env v ->
  match v with
  | TVar "Bool" -> Bool
  | TVar "Int" -> Int
  | TVar x -> TVar x
  | TLam (x, t) -> List (quote (extend env x (TVar x)) (t (TVar x)))
  | TArrow (a, b) -> Arrow ((quote env a), (quote env b))
  | TApp (a, b) -> Arrow ((quote env a), (quote env b))
