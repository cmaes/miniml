type t = string

let counter = ref 0
let genid s = incr counter;
              Printf.sprintf "%s.%d" s !counter

let rec id_of_type = function
  | Type.Unit -> "u"
  | Type.Bool  -> "b"
  | Type.Float -> "d"
  | Type.Fun _ -> "f"
  | Type.Var _ -> assert false

let gentmp typ =
  incr counter;
  Printf.sprintf "T%s%d" (id_of_type typ) !counter
