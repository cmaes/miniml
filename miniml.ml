
let repl _ =
  print_endline "MiniML. Press Ctrl-D to exit.";
  try
    while true do
        print_string "MiniML> ";
        let str = read_line () in
        let ast = Parser.exp Lexer.token (Lexing.from_string str) in
        print_endline (Prettyprint.string_of_expr ast);
        print_endline (Prettyprint.string_of_type (Typing.infer Env.empty ast))
    done
  with
    End_of_file -> print_endline "\nGoodbye."



let main =
  repl ()
