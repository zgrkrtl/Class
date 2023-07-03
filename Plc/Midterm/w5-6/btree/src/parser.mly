%{
    open Ast
%}

%token EOL LPAREN RPAREN COMMA USCORE
%token <int> NUM
%start <btree> start
%%

start:
  | t = S; EOL 
    { t };

S:
  | USCORE
    { Empty }
  | LPAREN; i = NUM; COMMA; l = S; COMMA; r = S; RPAREN
    { Node(i, l, r) }
