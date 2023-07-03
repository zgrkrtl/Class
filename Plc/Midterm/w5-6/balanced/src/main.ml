open Printf
open Lexing
open Lexer
open Ast
open Parser

let switchToken(t: Lexer.token): Parser.token =
  match t with
  | BLANK   -> BLANK
  | EOL     -> EOL
  | LPAREN  -> LPAREN
  | RPAREN  -> RPAREN
  | NUM i   -> NUM i
  | IDENT s -> IDENT s

let compose(f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  fun (x: 'a) -> g (f x) 

let astOfString(s: string): balanced =
  start (compose tokenize switchToken) (from_string s)

let main: unit =
  let t = tokenize (from_string "[[]]burak") in
  printToken t;
  let tl = getTokensFromString "][2023[]]burak" in
  printTokenList tl;
  let ast = astOfString "[][][[][][][[[[][]burak]][]][][]]" in
  printBalanced ast;
  let ast = astOfString "[8 9  5  10]" in
  printBalanced ast;