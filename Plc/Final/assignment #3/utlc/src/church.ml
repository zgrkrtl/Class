open Printf
open Terms
open Subst
open Beta
open Booleans

let lZero: term =
  Lambda("s", Lambda("z", Var "z"))

let lOne: term =
  Lambda("s", Lambda("z", App(Var "s",Var "z")))

let lTwo: term =
  Lambda("s", Lambda("z", App(Var "s",App(Var "s",Var "z"))))

let lThree: term =
  Lambda("s", Lambda("z", App(Var "s",App(Var "s", App(Var "s",Var "z")))))

let rec lNumeralH(i: int) (acc: term): term =
  if i < 0 then failwith "no negative natural number"
  else if i = 0 then acc
  else lNumeralH (i-1) (App(Var "s", acc))

let lNumeral (i: int): term =
  Lambda("s", Lambda("z", lNumeralH i (Var "z")))

let lAddH: term =
  Lambda("M", Lambda("N", Lambda("s", Lambda("z", App(App(Var "N",Var "s"),App(App(Var "M",Var "s"),Var "z"))))))

let lAdd (t1: term) (t2: term): term =
  App(App(lAddH,t1),t2)

let lMultH: term =
  Lambda("M", Lambda("N", Lambda("s", Lambda("z", App(App(Var "N",App(Var "M",Var "s")),Var "z")))))

let lMult (t1: term) (t2: term): term =
  App(App(lMultH,t1),t2)

let lPredH: term =
  Lambda("n", Lambda("s", Lambda("z", App(App(App(Var "n",Lambda("g", Lambda("h", App(Var "h",App(Var "g",Var "s"))))),Lambda("u", Var "z")),Lambda("u", Var "u")))))

let lPred (t: term): term =
  App(lPredH, t)

let lSubtrH: term =
  Lambda("m", Lambda("n", App(App(Var "n",lPredH),Var "m")))

let lSubtr (t1: term) (t2: term): term =
  App(App(lSubtrH,t1),t2)

let lIsZeroH: term =
  Lambda("n", App(App(Var "n",Lambda("x", lFalse)),lTrue))

let lIsZero (t: term): term =
  App(lIsZeroH, t)

let lLeqH: term =
  Lambda("m", Lambda("n", lIsZero (lSubtr (Var "m") (Var "n"))))

let lLeq (t1: term) (t2: term): term =
  App(App(lLeqH,t1),t2)

let lEqH: term =
  Lambda("m", Lambda("n", lAnd (lLeq (Var "m") (Var "n")) (lLeq (Var "n") (Var "m"))))

let lEq (t1: term) (t2: term): term =
  App(App(lEqH,t1),t2)