(* intermediate representation *)

type expr =
  | Unit
  | Bool of bool
  | Float of float
  | Neg of  expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Eq of  expr * expr
  | Le of expr * expr
  | If of expr * expr * expr
  | Let of (Id.t * Type.t) * expr * expr
  | Var of Id.t
  | LetRec of fundef * expr
  | App of Id.t * expr list
  | ExtFunApp of Id.t * expr list
and
  fundef = { name: Id.t * Type.t; args: (Id.t * Type.t) list; body: expr }

let rec normalize env = function
  | Syntax.Unit -> Unit, Type.Unit
  | Syntax.Bool b -> Bool b, Type.Bool
  | Syntax.Float f -> Float f, Type.Float
  | Syntax.Not(e) -> normalize env (Syntax.If(e, Syntax.Bool(false), Syntax.Bool(true)))
  | Syntax.Neg(e) -> let e', _ = normalize env e in
                     Neg(e'), Type.Float
  | Syntax.Add(e1, e2) -> let e1', _ = normalize env e1 in
                          let e2', _ = normalize env e2 in
                          Add(e1', e2'), Type.Float
  | Syntax.Sub(e1, e2) -> let e1', _ = normalize env e1 in
                          let e2', _ = normalize env e2 in
                          Sub(e1', e2'), Type.Float
  | Syntax.Mult(e1,e2) -> let e1', _ = normalize env e1 in
                          let e2', _ = normalize env e2 in
                          Mult(e1',e2'), Type.Float
  | Syntax.Div(e1,e2) -> let e1', _ = normalize env e1 in
                         let e2', _ = normalize env e2 in
                         Div(e1',e2'), Type.Float
  | Syntax.Eq(e1,e2) -> let e1', _ = normalize env e1 in
                        let e2', _ = normalize env e2 in
                        Eq(e1',e2'), Type.Bool
  | Syntax.Le(e1,e2) -> let e1', _ = normalize env e1 in
                        let e2', _ = normalize env e2 in
                        Le(e1',e2'), Type.Bool
  | Syntax.If(e1, e2, e3) -> let e1', _ = normalize env e1 in
                             let e2', t = normalize env e2 in
                             let e3', _ = normalize env e3 in
                             If (e1', e2', e3'), t
  | Syntax.Let ((x,t), e1, e2) -> let e1', t1 = normalize env e1 in
                                  let e2', t2 = normalize (Env.add x t env) e2 in
                                  Let ((x, t), e1', e2'), t2
  | Syntax.Var(x) when Env.mem x env -> Var(x), Env.find x env
  | Syntax.Var(x) -> failwith (Printf.sprintf "undefined external variable %s" x)
  | Syntax.LetRec({ Syntax.name = (x, t); Syntax.args = yts; Syntax.body = e1}, e2) ->
     let env' = Env.add x t env in
     let e2', t2 = normalize env' e2 in
     let e1', t1 = normalize (Env.add_list yts env') e1 in
     LetRec({name = (x, t); args = yts; body = e1'}, e2'), t2
  | Syntax.App(Syntax.Var(f), e2s) when not (Env.mem f env) ->
     (match Env.find f !Typing.extenv with
      | Type.Fun(_, t) -> let args = List.map (fun e -> fst (normalize env e)) e2s in
                             ExtFunApp (f, args), t
      | _ -> assert false)
  | Syntax.App(Syntax.Var(f), e2s) when (Env.mem f env)->
     let t = Env.find f env in
     let args = List.map (fun a -> fst (normalize env a)) e2s in
     App(f, args), t
  | Syntax.App(e1, e2s) ->
     let x, funtyp = normalize env e1 in
     (match funtyp with
      | Type.Fun(r, t) -> let f = Id.gentmp funtyp in
                          let args = List.map (fun a -> fst (normalize env a)) e2s in
                          Let((f, funtyp), x, App(f, args)), t
      | _ -> failwith ("Bad app type: " ^ Prettyprint.string_of_type funtyp))


let inter_rep e = fst (normalize (Env.empty) e)
