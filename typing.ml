open Syntax

exception Unify of Type.t * Type.t
exception Error of expr * Type.t * Type.t

let extenv = ref Env.empty

(* occur does an occurs check to see whether the the type
   variable appears inside the other type. It is necessary
   so that the resulting type has not cycle. For example,
   if we unified (with no occur check) type variable alpha
   with fucntion type int -> alpha, the result would be
   an infiite like like int->int->int->int->...,because
   alpha = int->alpha *)

let rec occur r1 = function
  | Type.Fun(t2s, t2) -> List.exists (occur r1) t2s || occur r1 t2
  | Type.Var(r2) when r1 == r2  -> true
  | Type.Var({contents = None}) -> false
  | Type.Var({contents = Some(t2)}) -> occur r1 t2
  | _ -> false

let rec unify t1 t2 =
  match t1, t2 with
  | Type.Unit, Type.Unit | Type.Bool, Type.Bool | Type.Float, Type.Float -> ()
  | Type.Fun(t1s, t1'), Type.Fun(t2s, t2') ->
     (try List.iter2 unify t1s t2s
      with Invalid_argument("List.iter2") -> raise (Unify (t1, t2)));
     unify t1' t2'
  | Type.Var(r1), Type.Var(r2) when r1 == r2 -> ()
  | Type.Var({ contents = Some(t1') }), _ -> unify t1' t2
  | _, Type.Var({ contents = Some(t2')}) -> unify t1 t2'
  | Type.Var({ contents = None } as r1), _ ->
     if occur r1 t2 then raise (Unify (t1, t2));
     r1 := Some t2
  | _, Type.Var({ contents = None} as r2) ->
     if occur r2 t1 then raise (Unify (t1, t2));
     r2 := Some (t1)
  | _, _ -> raise (Unify (t1, t2))

let rec infer env e =
  try
    match e with
    | Unit -> Type.Unit
    | Bool _ -> Type.Bool
    | Float _ -> Type.Float
    | Not (e) -> unify Type.Bool (infer env e);
                 Type.Bool
    | Neg (e) -> unify Type.Float (infer env e);
                 Type.Float
    | Add (e1, e2)
    | Sub (e1, e2)
    | Mult (e1, e2)
    | Div  (e1, e2) ->
       unify Type.Float (infer env e1);
       unify Type.Float (infer env e2);
       Type.Float
    | Eq (e1, e2) | Le (e1, e2) ->
       unify (infer env e1) (infer env e2);
       Type.Bool
    | If (p, c, a) ->
       unify (infer env p) Type.Bool;
       let t = infer env c in
       unify t (infer env a);
       t
    | Let ((x,t), e1, e2) ->
       unify t (infer env e1);
       infer (Env.add x t env) e2
    | Var (x) when Env.mem x env -> Env.find x env
    | Var (x) when Env.mem x !extenv -> Env.find x !extenv
    | Var (x) ->
       Format.eprintf "free variable %s assumed as external@." x;
       let t = Type.gentyp () in
       extenv := Env.add x t !extenv;
       t
    | LetRec ({name = (x, t); args = yts; body = e1}, e2) ->
       let env = Env.add x t env in
       unify t (Type.Fun(List.map snd yts, infer (Env.add_list yts env) e1));
       infer env e2
    | App (f, args) ->
       let t = Type.gentyp() in
       unify (infer env f) (Type.Fun(List.map (infer env) args, t));
       t
  with
    Unify (t1, t2) -> raise (Error(e, t1, t2))
