%{
open Syntax
let addtype x = (x, Type.gentyp ())
%}

%token <bool> BOOL
%token <float> FLOAT
%token <Id.t> IDENT
%token NOT
%token MINUS
%token PLUS
%token MULT
%token DIV
%token EQUAL
%token NOT_EQUAL
%token LESS_EQUAL
%token GREATER_EQUAL
%token EQUAL_EQUAL
%token LESS
%token GREATER
%token IF
%token THEN
%token ELSE
%token LET
%token IN
%token REC
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PREC_LET
%token PREC_IF
%token PREC_UNARY_MINUS
%token PREC_UNARY_NOT
%token PREC_APP
%token EOF

/* (* define associativity and precedence *) */
%right PREC_LET
%right SEMICOLON
%right PREC_IF
%left  EQUAL_EQUAL NOT_EQUAL LESS GREATER LESS_EQUAL GREATER_EQUAL
%left  PLUS MINUS
%left  MULT DIV
%right PREC_UNARY_MINUS
%right PREC_UNARY_NOT
%left  PREC_APP

%type <Syntax.expr> exp
%start exp

%%
exp:
    | e = expr; EOF { e }

simple_expr:
    | LPAREN expr RPAREN { $2 }
    | LPAREN RPAREN           { Unit }
    | BOOL                 { Bool $1 }
    | FLOAT                { Float $1 }
    | IDENT                { Var $1 }

expr:
    | simple_expr                                                { $1 }
    | NOT; expr                              { Not $2 } %prec PREC_UNARY_NOT
    | MINUS; expr                          { Neg $2 } %prec PREC_UNARY_MINUS
    | expr; PLUS; expr                                           { Add ($1, $3) }
    | expr; MINUS; expr                                          { Sub ($1, $3) }
    | expr; EQUAL_EQUAL; expr                                          { Eq ($1, $3) }
    | expr; NOT_EQUAL; expr                                { Not (Eq ($1, $3)) }
    | expr; LESS; expr                                     { Not (Le ($3, $1)) } /* (* a < b -> not (b <= a) *) */
    | expr; GREATER; expr                                  { Not (Le ($1, $3)) } /* (* a > b -> not (a <= b) *)*/
    | expr; LESS_EQUAL; expr                               { Le ($1, $3) }
    | expr; GREATER_EQUAL;  expr                            { Le($3, $1) } /* (* a >= b -> b <= a *) */
    | expr; MULT;   expr                                    { Mult ($1, $3) }
    | expr; DIV;   expr                                    { Div ($1, $3) }
    | IF; expr; THEN; expr; ELSE; expr     { If($2, $4, $6) } %prec PREC_IF
    | LET; IDENT; EQUAL; expr; IN; expr  { Let(addtype $2, $4, $6) } %prec PREC_LET
    | LET; REC; fundef; IN; expr;              { LetRec($3, $5) } %prec PREC_LET
    | expr; actual_args                               { App($1, $2) }
    | expr; SEMICOLON; expr                                { Let((Id.gentmp Type.Unit, Type.Unit), $1, $3) }

fundef:
    | IDENT; formal_args; EQUAL; expr  { { name=addtype $1; args = $2; body = $4 } }

formal_args:
    | IDENT; formal_args { (addtype $1):: $2 }
    | IDENT                  { [addtype $1] }


(*simple_expr_list:
    | s = simple_expr          { [s] }
    | s = simple_expr; t = simple_expr_list { s:: t }
 *)

actual_args:
    | actual_args; simple_expr  { $1 @ [$2] }  %prec PREC_APP
    | simple_expr                    { [$1] } %prec PREC_APP
