%{
    open Ast
%}

%token EOL LPAREN RPAREN LBRACE RBRACE PLUS MINUS MULT DIV AND OR EQ GT LT NEG SKIP INT ASSIGN SEQUENCE IF THEN ELSE WHILE
%left SEQUENCE
%left AND OR
%nonassoc NEG
%left PLUS MINUS 
%left MULT DIV
%token <bool> BOOL
%token <string> IDENT
%token <int> NUM
%start <cmd> start
%%

start:
  | c = cmd; EOL
    { c };

cmd:
  | SKIP
    { Skip }
  | INT;s = IDENT
    { Declare s }
  | s = IDENT; ASSIGN; a = aexpr
    { Assign(s, a) }
  | c1 = cmd; SEQUENCE; c2 = cmd
    { Sequence(c1, c2) }
  | IF; LPAREN; b = bexpr; RPAREN; THEN; LBRACE; c1 = cmd; RBRACE; ELSE; LBRACE; c2 = cmd; RBRACE
    { Ite(b, c1, c2) }
  | WHILE; LPAREN; b = bexpr; RPAREN; LBRACE; c1 = cmd; RBRACE
    { While(b, c1) };

aexpr:
  | i = NUM
    { Aconst i }
  | s = IDENT
    { Var s }
  | a1 = aexpr; PLUS; a2 = aexpr
    { Plus(a1, a2) }
  | a1 = aexpr; MINUS; a2 = aexpr
    { Minus(a1, a2) }
  | a1 = aexpr; MULT; a2 = aexpr
    { Mult(a1, a2) }
  | a1 = aexpr; DIV; a2 = aexpr
    { Div(a1, a2) };

bexpr:
  | b = BOOL
    { Bconst b }
  | b1 = bexpr; AND; b2 = bexpr
    { And(b1, b2) }
  | b1 = bexpr; OR; b2 = bexpr
    { Or(b1, b2) }
  | a1 = aexpr; EQ; a2 = aexpr
    { Eq(a1, a2) }
  | a1 = aexpr; GT; a2 = aexpr
    { Gt(a1, a2) }
  | a1 = aexpr; LT; a2 = aexpr
    { Lt(a1, a2) }
  | NEG; b = bexpr
    { Neg b }