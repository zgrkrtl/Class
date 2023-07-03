open Printf
open Terms
open Subst
open Beta
open Booleans
open Church
open YCombinator
open Pairs


let lConsH = Lambda("t1", Lambda("t2", lPair (lFalse) (lPair (Var "t1") (Var "t2"))))

let lCons (t1: term) (t2: term): term = App(App(lConsH,t1),t2)

let lNil = Lambda("l", Var "l")

let lHeadH = Lambda("l", lProj1 (lProj2 (Var "l")))

let lHead (t: term): term = App(lHeadH, t)

let lTailH = Lambda("l", lProj2 (lProj2 (Var "l")))

let lTail (t: term): term = App(lTailH, t)

let lIsNil = lProj1

let lLengthH: term =
  Lambda("f",
    Lambda("l", 
      lIte
        (lIsNil (Var "l"))
        (lNumeral 0)
        (lAdd (lNumeral 1) (App(Var "f",lTail (Var "l"))))))

let lLength (t: term): term =
  App(App(yComb, lLengthH), t)

let lReverseH: term =
  Lambda("f", Lambda("l1", Lambda("l2",
  lIte
  (lIsNil (Var "l1"))
  (Var "l2")
  (App(App(Var "f",(lTail (Var "l1"))),(lCons (lHead (Var "l1")) (Var "l2") ))))))

let lReverse (t: term): term =
  App(App(App(yComb,lReverseH),t),lNil)

let lAppendH: term =
  Lambda("k", Lambda("l1", Lambda("l2",
  lIte
  (lIsNil (Var "l1"))
  (Var "l2")
  (lCons (lHead (Var "l1")) (App(App(Var "k",lTail (Var "l1")),Var "l2")) )
  )))

let lAppend (t1: term) (t2: term): term =
  App(App(App(yComb,lAppendH),t1),t2)
