(* rename identifers to make them unique (alpha-conversion) *)

open Inter

let find x env = try Env.find x env with Not_found -> x

let rec rename env = function
  | Unit -> Unit
  | Bool b -> Bool b
  | Float f -> Float f
  | Neg e   -> Neg(rename env e)
  | Add(e1, e2) -> Add(rename env e1, rename env e2)
  | Sub(e1, e2) -> Sub(rename env e1, rename env e2)
  | Mult(e1, e2) -> Mult(rename env e1, rename env e2)
  | Div(e1, e2) -> Div(rename env e1, rename env e2)
  | Eq(e1, e2) -> Eq(rename env e1, rename env e2)
  | Le(e1, e2) -> Le(rename env e1, rename env e2)
  | If(e1, e2, e3) -> If (rename env e1, rename env e2, rename env e3)
  | Let ((x,t), e1, e2) ->
     let x' = Id.genid x in
     Let((x', t), rename env e1, rename (Env.add x x' env) e2)
  | Var(x) -> Var(find x env)
  | LetRec ( { name = (x, t); args = yts; body = e1 }, e2) ->
     let env = Env.add x (Id.genid x) env in
     let ys  = List.map fst yts in
     let env' = Env.add_list2 ys (List.map Id.genid ys) env in
     LetRec ( { name = (find x env, t);
                args = List.map (fun (x,t) -> (find x env', t)) yts;
                body = rename env' e1},
              rename env e2)
  | App ((f,t,tys), es) -> App ((find f env, t, tys), List.map (rename env) es)
  | ExtFunApp(x, es) -> ExtFunApp(x, List.map (rename env) es)

let rename_idents e = rename (Env.empty) e
