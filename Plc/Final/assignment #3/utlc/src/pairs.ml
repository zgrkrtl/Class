open Printf
open Terms
open Subst
open Beta
open Booleans
open Church
open YCombinator

let lPairH = Lambda("e1", Lambda("e2", Lambda("p", App(App(Var "p",Var "e1"),Var "e2"))))

let lPair (t1: term) (t2: term): term = App(App(lPairH,t1),t2)

let lProj1H = Lambda("u", App((Var "u"), lTrue))

let lProj1 (t: term): term = App(lProj1H, t)

let lProj2H = Lambda("u", App((Var "u"), lFalse))

let lProj2 (t: term): term = App(lProj2H, t)
