open Printf
open Types
open Terms
open Typecheck
open Subst
open Beta


let fibonacciH: term =
  Lambda("f",
         TArrow(TInt, TInt),
         Lambda("x",
                TInt,
                Ite(
                    IsEq(Var "x", ConstI 0),
                    ConstI 0,
                    Ite(
                        Leq(Var "x", ConstI 2),
                        ConstI 1,
                        Plus(App(Var "f",Minus(Var "x",ConstI 1)),
                             App(Var "f",Minus(Var "x",ConstI 2))
                            )
                       )
                   )
               )  
        )

let fibonacci(e: term): term =
  App(Fix fibonacciH, e)