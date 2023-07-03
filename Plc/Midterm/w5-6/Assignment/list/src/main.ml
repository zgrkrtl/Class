open Printf
open Lexing
open Lexer
open Ast
(* open Parser *)

(*

let switchTokens(t: Lexer.token): Parser.token =
  ... your code here ...

let compose(f: 'a -> 'b) (g: 'b -> 'c): 'a -> 'c = fun (x: 'a) -> g (f x)

let astOfString(s: string): llist =
  ... your code here ...

*)

let main: unit =
  let tl = getTokensFromString "[1,2,3,5]" in
  printTokenList tl;
(*
  let l1 = astOfString "[ 7, 98,4  ,8   ,   89  ]" in
  printLlist l1;     (* prints [7,98,4,8,89] *)
  let l2 = append l1 l1 in
  printLlist l2;     (* prints [7,98,4,8,89,7,98,4,8,89] *)
  let l3 = reverse l2 in
  printLlist l3;     (* prints [89,8,4,98,7,89,8,4,98,7] *)
  let v = length l3 in 
  printf "%d\n" v;   (* prints 10 *)
*)