open Printf
open Types
open Terms
open Typecheck
open Subst
open Beta


let factorialH: term =
  Lambda("f", 
         TArrow(TInt, TInt),
         Lambda("x",
                TInt,
                Ite(
                    IsEq(Var "x", ConstI 0),
                    ConstI 1,
                    Mult(Var "x",App(Var "f",Minus(Var "x",ConstI 1)))
                   )
               )
         )

let factorial(e: term): term =
  App(Fix factorialH, e)


  