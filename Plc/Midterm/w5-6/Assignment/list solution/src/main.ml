open Printf
open Lexing
open Lexer
open Ast
open Parser

let switchTokens(t: Lexer.token): Parser.token =
  match t with
    | LPAREN -> LPAREN
    | RPAREN -> RPAREN
    | COMMA  -> COMMA
    | EOL    -> EOL
    | NUM i  -> NUM i

let compose(f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  fun (x: 'a) -> g (f x)

let astOfString(s: string): llist =
  start (compose tokenize switchTokens) (from_string s)

let main: unit =
  let tl = getTokensFromString "[1,2,3,5]" in
  printTokenList tl;
  let l1 = astOfString "[ 7, 98,4  ,8   ,   89  ]" in
  printLlist l1;
  let l2 = append l1 l1 in
  printLlist l2;
  let l3 = reverse l2 in
  printLlist l3;
  let v = length l3 in 
  printf "%d\n" v;
