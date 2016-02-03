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

let rec pp_list = function
  | [] -> ""
  | [x] -> x
  | x :: xs -> x ^ " " ^ pp_list xs

let rec string_of_expr e = to_str (-1) e
  and
    to_str n e =
    let (m, str) = match e with
      | Unit ->   (8, "()")
      | Bool b -> (8, string_of_bool b)
      | Float f -> (8, Printf.sprintf "%g" f)
      | Var   i -> (8, "var:" ^ i)
      | App  (f, elst) -> (7, "apphead: " ^(to_str (-1) f) ^ " appargs: " ^ pp_list (List.map (to_str (-1)) elst))
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
      | LetRec ( { Syntax.name = f; Syntax.args = a; Syntax.body = e1}, e2) ->
         (0, "let rec " ^ (fst f) ^ " " ^ pp_list (List.map fst a)  ^ " = " ^
               (to_str (-1) e1) ^ " in " ^ (to_str (-1) e2))
    in
    if m < n then "(" ^ str ^ ")" else str


let rec string_of_type = function
  | Type.Unit -> "unit"
  | Type.Bool  -> "bool"
  | Type.Float -> "float"
  | Type.Fun (ts, t) -> "(" ^ pp_list (List.map (string_of_type) ts) ^ " -> " ^ (string_of_type t) ^ ")"
  | Type.Var({ contents = Some t }) -> "var-some: " ^ string_of_type t
  | Type.Var({ contents = None }) -> "unknown"


let rec string_of_inter e = i_to_str (-1) e
  and
    i_to_str n e =
    let (m, str) = match e with
      | Inter.Unit ->   (8, "()")
      | Inter.Bool b -> (8, string_of_bool b)
      | Inter.Float f -> (8, Printf.sprintf "%g" f)
      | Inter.Var   i -> (8, "var:" ^ i)
      | Inter.Neg e ->   (6, "-" ^ (i_to_str 6 e) )
      | Inter.Mult (e1, e2) -> (5, (i_to_str 5 e1) ^ " * " ^ (i_to_str 6 e2))
      | Inter.Div  (e1, e2) -> (5, (i_to_str 5 e1) ^ " / " ^ (i_to_str 6 e2))
      | Inter.Add  (e1, e2) -> (4, (i_to_str 4 e1) ^ " + " ^ (i_to_str 5 e2))
      | Inter.Sub  (e1, e2) -> (4, (i_to_str 4 e1) ^ " - " ^ (i_to_str 5 e2))
      | Inter.Le   (e1, e2) -> (3, (i_to_str 3 e1) ^ " <= " ^ (i_to_str 4 e2))
      | Inter.Eq   (e1, e2) -> (2, (i_to_str 2 e1) ^ " == " ^ (i_to_str 3 e2))
      | Inter.If   (p, c, a) -> (1, "if P:" ^ (i_to_str (-1) p) ^ " then C:" ^ (i_to_str (-1) c) ^ " else A:" ^ (i_to_str (-1) a))
      | Inter.Let  ((i, t), e1, e2) -> (0, "let " ^ i ^ " = " ^ (i_to_str (-1) e1) ^ " in " ^ (i_to_str (-1) e2))
      | Inter.LetRec ( { Inter.name = f; Inter.args = a; Inter.body = e1}, e2) ->
         (0, "let rec " ^ (fst f) ^ " " ^ pp_list (List.map (fst) a)  ^ " = " ^
               (i_to_str (-1) e1) ^ " in " ^ (i_to_str (-1) e2))
      | Inter.App  ((f, t, tys), elst) -> (7, "apphead: " ^  f ^ " appargs: " ^ pp_list (List.map (i_to_str (-1)) elst))
      | Inter.ExtFunApp (f, elst) -> (7, "eapphead: " ^  f ^ " appargs: " ^ pp_list (List.map (i_to_str (-1)) elst))
    in
    if m < n then "(" ^ str ^ ")" else str


let rec string_of_closure_expr e = c_to_str (-1) e
  and
    c_to_str n e =
    let (m, str) = match e with
      | Closure.Unit ->   (8, "()")
      | Closure.Bool b -> (8, string_of_bool b)
      | Closure.Float f -> (8, Printf.sprintf "%g" f)
      | Closure.Var   i -> (8, "var:" ^ i)
      | Closure.Neg e ->   (6, "-" ^ (c_to_str 6 e) )
      | Closure.Mult (e1, e2) -> (5, (c_to_str 5 e1) ^ " * " ^ (c_to_str 6 e2))
      | Closure.Div  (e1, e2) -> (5, (c_to_str 5 e1) ^ " / " ^ (c_to_str 6 e2))
      | Closure.Add  (e1, e2) -> (4, (c_to_str 4 e1) ^ " + " ^ (c_to_str 5 e2))
      | Closure.Sub  (e1, e2) -> (4, (c_to_str 4 e1) ^ " - " ^ (c_to_str 5 e2))
      | Closure.Le   (e1, e2) -> (3, (c_to_str 3 e1) ^ " <= " ^ (c_to_str 4 e2))
      | Closure.Eq   (e1, e2) -> (2, (c_to_str 2 e1) ^ " == " ^ (c_to_str 3 e2))
      | Closure.If   (p, c, a) ->
         (1, "if P:" ^ (c_to_str (-1) p) ^ " then C:" ^ (c_to_str (-1) c) ^ " else A:" ^ (c_to_str (-1) a))
      | Closure.Let  ((i, t), e1, e2) -> (0, "let " ^ i ^ " = " ^ (c_to_str (-1) e1) ^ " in " ^ (c_to_str (-1) e2))
      | Closure.MakeCls ((x,t), {Closure.entry = x'; Closure.actual_fv = fvs}, e) ->
         (7, "makecls (" ^ x ^ ", {" ^ x' ^ " , " ^ (pp_list (List.map fst fvs)) ^ " }, " ^  (c_to_str (-1) e) ^ ")")
      | Closure.AppCls  ((f, t, tys), elst) -> (7, "appcls: " ^  f ^ " appargs: " ^ pp_list (List.map (c_to_str (-1)) elst))
      | Closure.AppDir (f, elst) -> (7, "appdir: " ^  f ^ " appargs: " ^ pp_list (List.map (c_to_str (-1)) elst))
    in
    if m < n then "(" ^ str ^ ")" else str


let string_of_closure_fundef { Closure.name = (x, t);
                               Closure.args = yts;
                               Closure.formal_fv = fvs;
                               Closure.takes_closure = tc;
                               Closure.body = e } =
  "fundef: " ^ x ^ " funtyp: " ^ string_of_type t ^ " args:" ^ pp_list (List.map fst yts) ^ " {env: " ^ pp_list (List.map fst fvs) ^ "} takes_closure: " ^ string_of_bool tc ^ "  body: " ^ string_of_closure_expr e

let string_of_prog = function
  | Closure.Prog (defs, e) ->
     let defs_str = List.fold_left (fun acc f -> acc ^ string_of_closure_fundef f ^ "\n") "" defs in
     let e_str = string_of_closure_expr e in
     defs_str ^ ";; " ^ e_str
