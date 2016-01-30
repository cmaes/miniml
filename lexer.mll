{
  open Lexing
  open Parser
  open Type
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0' - '9']
let frac  = '.' digit*
let exp   = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let ident = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
 | space+  { token lexbuf }
 | "(*"    { comment lexbuf; token lexbuf }
 | '('     { LPAREN }
 | ')'     { RPAREN }
 | "true"  { BOOL(true) }
 | "false" { BOOL(false) }
 | "not"   { NOT }
 | float   { FLOAT (float_of_string (lexeme lexbuf)) }
 | ident   { IDENT (lexeme lexbuf) }
 | '-'     { MINUS }
 | '+'     { PLUS }
 | '*'     { MULT }
 | '/'     { DIV }
 | '='     { EQUAL }
 | "!="    { NOT_EQUAL }
 | "<="    { LESS_EQUAL }
 | ">="    { GREATER_EQUAL }
 | '<'     { LESS }
 | '>'     { GREATER }
 | "if"    { IF }
 | "then"  { THEN }
 | "else"  { ELSE }
 | "let"   { LET }
 | "rec"   { REC }
 | "in"    { IN }
 | "_"     { IDENT(Id.gentmp Type.Unit) }
 | ";"     { SEMICOLON }
 | eof     { EOF }

and comment = parse
  | "*)"   { () }
  | "(*"   { comment lexbuf; comment lexbuf }
  | eof    { Format.eprintf "warning unterminated comment@." }
  | _      { comment lexbuf }
