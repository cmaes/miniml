let prog_of_lex lex =
  let ast = Parser.exp Lexer.token lex in

  print_endline "AST:";
  print_endline (Prettyprint.string_of_expr ast);

  let ast_typ = Typing.inference ast in
  let inter = Inter.inter_rep ast_typ in

  print_endline "INTER:";
  print_endline (Prettyprint.string_of_inter inter);

  let alpha = Alpha.rename_idents inter in

  print_endline "ALPHA:";
  print_endline (Prettyprint.string_of_inter alpha);

  let prog  = Closure.closure_convert alpha in

  print_endline "PROG:";
  print_endline (Prettyprint.string_of_prog prog);

  print_endline "external functions:";
  Env.iter (fun name typ -> print_endline (name ^ " : " ^ Prettyprint.string_of_type typ)) !Typing.extenv;

  prog

let extern_fun _ =
  Env.fold (fun name typ acc -> (name, typ) :: acc) (!Typing.extenv) []

let repl _ =
  print_endline "MiniML. Press Ctrl-D to exit.";
  try
    while true do
        print_string "MiniML> ";
        let str = read_line () in
        ignore (prog_of_lex (Lexing.from_string str));
    done
  with
    End_of_file -> print_endline "\nGoodbye."

exception Badfilename of string
let bad_filename msg = raise (Badfilename msg)

let get_last_period str =
  let last_period = ref (-1) in
  for i = 0 to (String.length str) - 1 do
    if str.[i] == '.' then
      last_period := i
    else
      ()
  done;
  if !last_period >= 0 then
    !last_period
  else
    bad_filename "No period found in filename"

let basename fname =
  let last_period = get_last_period fname in
  String.sub fname 0 last_period



let main =
  if Array.length Sys.argv > 1 then
    let fname  = Sys.argv.(1) in
    let ic     = open_in fname in
    let lexbuf = Lexing.from_channel ic in
    let prog   =  prog_of_lex lexbuf in
    let outfname = basename Sys.argv.(1) ^ ".ll" in
    let externlst = extern_fun () in
    Compile.compile outfname prog externlst
  else
    repl ()
