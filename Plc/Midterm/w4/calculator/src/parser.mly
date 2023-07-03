%{
    open Ast
%}

%token BLANK LPAREN RPAREN EOL PLUS MULT MINUS DIV
%token <int> NUM
%token <string> IDENT
%start <expr> init
%%

init:
  | a = S; EOL
    { a };

S:
  | a = S; PLUS; b = T
    { Plus(a, b) }
  | a = S; MINUS; b = T
    { Minus(a, b) }
  | a = T
    { a };

T:
  | a = T; MULT; b = U
    { Mult(a, b) }
  | a = T; DIV; b = U
    { Div(a, b) }
  | a = U
    { a };

U:
  | a = NUM
    { Num a }
  | LPAREN; a = S; RPAREN
    { Par a }
  | BLANK
    { Ident "" }
  | a = IDENT
    { Ident a }