open Printf
open Lexing
open Lexer
open Ast
open Parser

let switchTokens(t: Lexer.token): Parser.token =
  match t with
  | BLANK   -> BLANK
  | LPAREN  -> LPAREN
  | RPAREN  -> RPAREN
  | EOL     -> EOL
  | IDENT s -> IDENT s
  | NUM i   -> NUM i
  | PLUS    -> PLUS
  | MINUS   -> MINUS
  | MULT    -> MULT
  | DIV     -> DIV

let compose(f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c =
  fun (x: 'a) -> g (f x)

let astOfString(s: string): expr =
  init (compose tokenize switchTokens) (from_string s)


let main: unit =
  let tl = getTokensFromString "(3-9)     +8/2" in
  printTokenList tl;
  let ast = astOfString "3-9*8+2" in
  printExpr ast;
  let v = eval ast in
  printf "%d\n" v;