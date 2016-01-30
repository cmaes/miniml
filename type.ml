type t =
  | Unit
  | Bool
  | Float
  | Fun of t list * t
  | Var of t option ref

let gentyp () = Var (ref None)
