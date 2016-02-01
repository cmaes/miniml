type closure = { entry: Id.t; actual_fv: Id.t list }

type expr =
  | Unit
  | Bool of bool
  | Float of float
  | Neg of expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mult of expr * expr
  | Div of expr * expr
  | Eq of expr * expr
  | Le of expr * expr
  | If of expr * expr * expr
  | Let of (Id. t * Type.t) * expr * expr
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * expr
  | AppCls of Id.t * expr list
  | AppDir of Id.t * expr list

type fundef = { name: Id.t * Type.t;
                args: (Id.t * Type.t) list;
                formal_fv: (Id.t * Type.t) list;
                body: expr }
type prog = Prog of fundef list * expr

let toplevel : fundef list ref = ref []


let rec freevar = function
  | Unit | Bool _ | Float _  -> S.empty
  | Neg (e) -> freevar e
  | Add(e1, e2) | Sub(e1, e2) | Mult(e1, e2) | Div(e1, e2)  | Eq(e1, e2) | Le(e1, e2)  -> S.union (freevar e1) (freevar e2)
  | If(e1, e2, e3) -> S.union (freevar e1) (S.union (freevar e1) (freevar e2))
  | Let((x, t), e1, e2) -> S.union (freevar e1) (S.remove x (freevar e2))
  | Var(x) -> S.singleton x
  | MakeCls((x,t), {entry = l; actual_fv = ys}, e) -> S.remove x (S.union (S.of_list ys) (freevar e))
  | AppCls (x, es) -> S.add x (List.fold_left (fun s e -> S.union (freevar e) s) S.empty es)
  | AppDir (_, es) -> List.fold_left (fun s e -> S.union (freevar e) s) S.empty es


(* helper function for printing a list *)
let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let rec convert env known = function
  | Inter.Unit -> Unit
  | Inter.Bool b -> Bool b
  | Inter.Float f -> Float f
  | Inter.Neg e -> Neg (convert env known e)
  | Inter.Add(e1, e2) -> Add(convert env known e1, convert env known e2)
  | Inter.Sub(e1, e2) -> Sub(convert env known e1, convert env known e2)
  | Inter.Mult(e1, e2) -> Mult(convert env known e1, convert env known e2)
  | Inter.Div(e1, e2) -> Div(convert env known e1, convert env known e2)
  | Inter.Eq(e1, e2) -> Eq(convert env known e1, convert env known e2)
  | Inter.Le(e1, e2) -> Le(convert env known e1, convert env known e2)
  | Inter.If(e1, e2, e3) -> If(convert env known e1, convert env known e2, convert env known e3)
  | Inter.Let((x,t), e1, e2) -> Let((x,t), convert env known e1, convert (Env.add x t env) known e2)
  | Inter.Var(x) -> Var(x)
  | Inter.LetRec({ Inter.name = (x, t); Inter.args = yts; Inter.body = e1 }, e2) ->
     let toplevel_backup = !toplevel in (* make a backup in case we need to revert *)
     let env' = Env.add x t env in (* add x to env for recursive functions *)
     let known' = S.add x known in (* add x to the know functions *)
     let e1' = convert (Env.add_list yts env') known' e1 in
     let free_vars = S.diff (freevar e1') (S.of_list (List.map fst yts)) in
     let known', e1' =
       if S.is_empty free_vars then known', e1' else
         (* body of the function contains free variables *)
         (Format.eprintf "free variable(s) %s found in function %s@." (pp_list (S.elements free_vars)) x;
          Format.eprintf "function %s cannot be directly applied@. " x;
          (* revert toplevel to the backup, removing the current function *)
          toplevel := toplevel_backup;
          let e1' = convert (Env.add_list yts env') known e1 in
          known, e1') in
     let free_vars_lst = S.elements (S.diff (freevar e1') (S.add x (S.of_list (List.map fst yts)))) in
     let free_var_typs = List.map (fun z -> (z, Env.find z env')) free_vars_lst in
     toplevel := { name = (x, t); args = yts; formal_fv = free_var_typs; body = e1'} :: !toplevel;
     let e2' = convert env' known' e2 in
     if S.mem x (freevar e2') then
       MakeCls( (x,t), { entry = x; actual_fv = free_vars_lst}, e2')
     else
       (Format.eprintf "eliminating closure(s) %s@." x;
        e2')
  | Inter.App(f, es) when S.mem f known ->
     Format.eprintf "directly applying %s@." f;
     AppDir(f, List.map (convert env known) es)
  | Inter.App(f, es) -> AppCls(f, List.map (convert env known) es)
  | Inter.ExtFunApp(f, es) -> AppDir("miniml_" ^ f, List.map (convert env known) es)


let closure_convert e =
  toplevel := [];
  let e' = convert Env.empty S.empty e in
  Prog(List.rev !toplevel, e')
