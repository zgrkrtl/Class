open Printf
open Ast
open Eval

(*
int a;
while (a < 5)
{
  a := a + 1;
  skip
} 
*)

let main: unit =
  let ae = Plus(Var "a", Aconst 1) in
  let be = Lt(Var "a", Aconst 5) in
  let c  = Sequence(Declare "b", Sequence(Declare "a", While(be, Assign("a", ae)))) in
  let ec = evalCmd (Sequence(c, (Assign("b", (Var "a"))))) [] in
  printState ec;