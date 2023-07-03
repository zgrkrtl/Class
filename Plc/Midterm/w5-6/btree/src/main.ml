open Printf
open Ast
open Lexing
open Lexer
open Parser

let twistToken(t: Lexer.token): Parser.token =
  match t with
  | EOL    -> EOL
  | LPAREN -> LPAREN
  | RPAREN -> RPAREN
  | COMMA  -> COMMA
  | USCORE -> USCORE
  | NUM i  -> NUM i

let compose(f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  fun (x: 'a) -> g(f x)

let btreeOfString(s: string): btree =
  start (compose tokenize twistToken) (from_string s)

let main: unit =
  let t = Node(7,Node(5, Empty, Empty),Node(9, Empty, Empty)) in
  printBtree t;
  let t = btreeOfString "[7,[5,[2,_,_],[6,_,_]],[9,_,_]]" in
  printBtree t;
  let l = flatten t in
  printIntList l;
  let s = size t in
  printf "%d\n" s;
