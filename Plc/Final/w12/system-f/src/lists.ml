open Printf
open Types
open Terms
open Typecheck
open Subst
open Beta

let lengthH: term =
  PLambda("a",
          Lambda("f",
                 TArrow(List(TVar "a"),TInt),
                 Lambda("l",
                        List(TVar "a"),
                        Ite(IsEq(Var "l", Nil(TVar "a")),
                            ConstI 0,
                            Plus(ConstI 1, App(Var "f",Tail(Var "l")))
                           )
                        )
                )
         )

let length (l: term) (t: myType): term =
  App(Fix(TApp(lengthH, t)), l)

let appendH: term =
  PLambda("a",
          Lambda("f",
                 TArrow(List(TVar "a"), TArrow(List(TVar "a"),List(TVar "a"))),
                 Lambda("l1",
                        List(TVar "a"),
                        Lambda("l2",
                               List(TVar "a"),
                               Ite(IsEq(Var "l1", Nil(TVar "a")),
                                   Var "l2",
                                   Cons(Head(Var "l1"), App(App(Var "f",Tail(Var "l1")),Var "l2"))
                                  )
                              )
                       )
                )
         )

let append (l1: term) (l2: term) (t: myType) =
  App(App(Fix(TApp(appendH, t)), l1), l2)