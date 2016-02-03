type closure = { entry: Id.t; actual_fv: (Id.t * Type.t) list }

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
  | AppCls of (Id.t * Type.t * Type.t list) * expr list
  | AppDir of Id.t * expr list

type fundef = { name: Id.t * Type.t;
                args: (Id.t * Type.t) list;
                formal_fv: (Id.t * Type.t) list;
                mutable takes_closure: bool;
                body: expr }
type prog = Prog of fundef list * expr

val closure_convert : Inter.expr -> prog
