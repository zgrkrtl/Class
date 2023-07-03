open Printf
open Terms
open Subst
open Beta
open Booleans
open Church

let yComb: term =
  Lambda("f", App( 
                 Lambda("x", App(Var "f",App(Var "x",Var "x"))),
                 Lambda("x", App(Var "f",App(Var "x",Var "x")))
                 )
         )