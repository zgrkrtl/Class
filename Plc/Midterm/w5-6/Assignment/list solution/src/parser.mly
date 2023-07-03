%{
    open Ast
%}

%token LPAREN RPAREN COMMA EOL
%token <int> NUM
%start <llist> start
%%

start:
  | a = S; EOL
    { a };

S:
  | LPAREN; a = T; RPAREN
    { a }
  | LPAREN; RPAREN
    { Nil };

T:
  | x = NUM; COMMA; xs = T
    { Cons(x, xs) }
  | x = NUM
    { Cons(x, Nil) };