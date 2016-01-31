open Id
open Syntax


(* precedence:

   0 | let let rec | definitions
   1 | if          | conditional
   2 | ==          | equality operatiors
   3 | <=          | comparison operators
   4 | +  -        | addition, subtraction
   5 | *  /        | multiplication, division
   6 |  - !        | unary minus, logical not
   7 | f e1 e2 e3  | application
 *)

let rec string_of_expr e = to_str (-1) e
  and
    to_str n e =
    let (m, str) = match e with
      | Unit ->   (8, "()")
      | Bool b -> (8, string_of_bool b)
      | Float f -> (8, Printf.sprintf "%g" f)
      | Var   i -> (8, "var:" ^ i)
      | App  (f, elst) -> (7, "apphead: " ^(to_str (-1) f) ^ " appargs: " ^ (List.fold_left (fun acc e -> acc ^ " " ^ (to_str (-1) e)) "" elst))
      | Not e ->   (6, "not " ^ (to_str 6 e ) )
      | Neg e ->   (6, "-" ^ (to_str 6 e) )
      | Mult (e1, e2) -> (5, (to_str 5 e1) ^ " * " ^ (to_str 6 e2))
      | Div  (e1, e2) -> (5, (to_str 5 e1) ^ " / " ^ (to_str 6 e2))
      | Add  (e1, e2) -> (4, (to_str 4 e1) ^ " + " ^ (to_str 5 e2))
      | Sub  (e1, e2) -> (4, (to_str 4 e1) ^ " - " ^ (to_str 5 e2))
      | Le   (e1, e2) -> (3, (to_str 3 e1) ^ " <= " ^ (to_str 4 e2))
      | Eq   (e1, e2) -> (2, (to_str 2 e1) ^ " == " ^ (to_str 3 e2))
      | If   (p, c, a) -> (1, "if P:" ^ (to_str (-1) p) ^ " then C:" ^ (to_str (-1) c) ^ " else A:" ^ (to_str (-1) a))
      | Let  ((i, t), e1, e2) -> (0, "let " ^ i ^ " = " ^ (to_str (-1) e1) ^ " in " ^ (to_str (-1) e2))
      | LetRec ( { name = f; args = a; body = e1}, e2) ->
         (0, "let rec " ^ (fst f) ^  (List.fold_left (fun acc e -> acc ^ " " ^ (fst e)) "" a) ^ " = " ^
               (to_str (-1) e1) ^ " in " ^ (to_str (-1) e2))
    in
    if m < n then "(" ^ str ^ ")" else str


let rec string_of_type = function
  | Type.Unit -> "unit"
  | Type.Bool  -> "bool"
  | Type.Float -> "float"
  | Type.Fun (ts, t) -> (List.fold_left (fun acc tt -> acc ^ (string_of_type tt) ^ "->" ) "" ts) ^ (string_of_type t)
  | Type.Var({ contents = Some t }) -> string_of_type t
  | Type.Var({ contents = None }) -> "external"
