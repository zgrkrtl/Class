%{
    open Ast
%}

%token BLANK LPAREN RPAREN EOL 
%token <string> IDENT
%token <int> NUM
%start <balanced> start
%%

start:
  | a = S; EOL
    { a };

S:
  | a = T
    { a };

T:
  | a = T; b = U
    { App(a, b) }
  | a = U
    { a };

U:
  | LPAREN; RPAREN
    { Const }
  | LPAREN; a = T; RPAREN
    { Uni a }
  | s = IDENT
    { Ident s }
  | i = NUM
    { Num i }
  | BLANK
    { Ident "" }

