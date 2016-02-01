
let repl _ =
  print_endline "MiniML. Press Ctrl-D to exit.";
  try
    while true do
        print_string "MiniML> ";
        let str = read_line () in
        let ast = Parser.exp Lexer.token (Lexing.from_string str) in

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
        print_endline (Prettyprint.string_of_prog prog)
    done
  with
    End_of_file -> print_endline "\nGoodbye."



let main =
  repl ()
