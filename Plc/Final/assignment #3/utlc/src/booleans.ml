open Printf
open Terms
open Subst
open Beta

let lTrue: term =
 Lambda("a", Lambda("b", Var "a"))

let lFalse: term =
 Lambda("a", Lambda("b", Var "b"))

let lNotH: term =
  Lambda("a", Lambda("b", Lambda("c", App(App(Var "a",Var "c"),Var "b"))))

let lNot (t: term): term =
  App(lNotH, t)

let lAndH: term =
  Lambda("a", Lambda("b", App(App(Var "a",Var "b"),Var "a")))

let lAnd (t1: term) (t2: term): term =
  App(App(lAndH,t1),t2)


let lOrH: term =
  Lambda("a", Lambda("b", App(App(Var "a",Var "a"),Var "b")))

let lOr (t1: term) (t2: term): term =
  App(App(lOrH,t1),t2)


let lIteH: term =
  Lambda("a", Lambda("b", Lambda("c", App(App(Var "a",Var "b"),Var "c"))))

let lIte (t1: term) (t2: term) (t3: term): term =
    App(App(App(lIteH,t1),t2),t3)


