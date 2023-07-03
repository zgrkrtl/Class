open Printf
open Ast
open Eval
open Lexing
open Lexer
open Parser
open In_channel

(*
int a;
while (a < 5)
{
  a := a + 1;
  skip
} 
*)

let twistToken(t: Lexer.token): Parser.token =
  match t with
  | EOL      -> EOL  
  | LPAREN   -> LPAREN
  | RPAREN   -> RPAREN
  | LBRACE   -> LBRACE
  | RBRACE   -> RBRACE
  | IDENT s  -> IDENT s
  | BOOL b   -> BOOL b
  | NUM i    -> NUM i
  | PLUS     -> PLUS
  | MINUS    -> MINUS
  | MULT     -> MULT
  | DIV      -> DIV
  | AND      -> AND
  | OR       -> OR
  | EQ       -> EQ
  | GT       -> GT
  | LT       -> LT
  | NEG      -> NEG
  | SKIP     -> SKIP
  | INT      -> INT
  | ASSIGN   -> ASSIGN
  | SEQUENCE -> SEQUENCE
  | IF       -> IF
  | THEN     -> THEN
  | ELSE     -> ELSE
  | WHILE    -> WHILE

let compose(f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  fun (x: 'a) -> g(f x)

let cmdOfString(s: string): cmd =
  start (compose tokenize twistToken) (from_string s)

let readFile(filename: string): string =
  let ic = open_gen [Open_rdonly] 777 filename in
  let s  = input_all ic in s 

let main: unit =
  let ae = Plus(Var "a", Aconst 1) in
  let be = Lt(Var "a", Aconst 5) in
  let c  = Sequence(Declare "b", Sequence(Declare "a", While(be, Assign("a", ae)))) in
  let ec = evalCmd (Sequence(c, (Assign("b", (Var "a"))))) [] in
  printState ec;
  let tl = getTokensFromString
  "int a;
   while (a < 5)
   {
     a := a + 1;
     skip
   } 
  "
  in
  printTokenList tl;
  let p = cmdOfString
  "
     int a;
     int res;
     a := 6;
     res := 1;
     while(a > 1)
     {
        res := res * a;
        a   := a - 1;
        skip
     }
  "
  in
  let st = evalCmd p [] in
  printState st;
  let s = readFile ((Sys.getcwd()) ^ "/src/p.imp") in
  let p = cmdOfString s in
  let st = evalCmd p [] in
  printState st;
