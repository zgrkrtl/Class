open Printf
open Terms
open Subst
open Beta
open Booleans
open Church
open YCombinator

let lFactorialH: term =
  Lambda("fact", Lambda("i",
  lIte
  (lIsZero (Var "i"))
  (lNumeral 1)
  (lMult (Var "i") (App(Var "fact",lPred (Var "i"))))
  ))

let lFactorial (t: term): term =
  App(App(yComb,lFactorialH),t)