%{
    open Ast
%}


%token BLANK LPAREN RPAREN COMMA EOL
%token <int> NUM
%start <llist> start
%%

(*

start:
  |  ... grammar (production rules) for your parser here ... 

*)