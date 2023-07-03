open Printf
open Types
open Terms
open Typecheck
open Subst
open Beta
open Factorial
open Fibonacci
open Lists

let main: unit =
  let t = TArrow(TBool, TInt) in
  printMyType t;
  let e = Lambda("x", TInt, Var "x") in
  printTerm e;
  let e = Plus(Var "x", Var "y") in
  printTerm e;
  let e = Ite(ConstB true, Plus(Var "x", Var "y"), Minus(Var "x", Var "y")) in
  printTerm e;
  let e = Lambda("z",
                 TProd(TArrow(TVar "A",TVar "B"),TArrow(TVar "A",TVar "C")),
                 Lambda("x",
                        TVar "A",
                        Pair(App(Fst(Var "z"),Var "x"),App(Snd(Var "z"),Var "x"))
                       )
                )
  in
  let t = typecheck [] e in
  (
    match t with
    | Yes t' -> printMyType t'
    | No s   -> printf "%s\n" s
  );
  let e = Lambda("z",
                 TArrow(TVar "A", TProd(TVar "B",TVar "C")),
                 Lambda("x",
                        TVar "A",
                        Pair(App(Fst(Var "z"),Var "x"),App(Snd(Var "z"),Var "x"))
                       )
                )
  in
  let t = typecheck [] e in
  (
    match t with
    | Yes t' -> printMyType t'
    | No s   -> printf "%s\n" s
  );
  let e = Lambda("z",
                 TArrow(TVar "A", TProd(TVar "B",TVar "C")),
                 Pair(Lambda("x", TVar "A", Fst(App(Var "z",Var "x"))),
                      Lambda("y", TVar "A", Snd(App(Var "z",Var "y")))
                     )
                )
  in
  let t = typecheck [] e in
  (
    match t with
    | Yes t' -> printMyType t'
    | No s   -> printf "%s\n" s
  );
  let e = Plus(ConstI 3, ConstI 5) in
  let t = typecheck [] e in
  (
    match t with
    | Yes t' -> printMyType t'
    | No s   -> printf "%s\n" s
  );
  let e = Plus(ConstI 3, ConstB true) in
  let t = typecheck [] e in
  (
    match t with
    | Yes t' -> printMyType t'
    | No s   -> printf "%s\n" s
  );

  let e = Lambda("x",
                 TVar "A",
                 App(Var "g",App(Var "f",Var "x"))
                )
  in
  let t = typecheck [("g",TArrow(TVar "B", TVar "C")); ("f",TArrow(TVar "A", TVar "B"))] e in
  (
    match t with
    | Yes t' -> printMyType t'
    | No s   -> printf "%s\n" s
  );

  let e = factorial (ConstI 3) in
  let e' = refl_trans_typed_beta [] e in
  printTerm e';
  let e = fibonacci (ConstI 10) in
  let e' = refl_trans_typed_beta [] e in
  printTerm e';
  let l = Cons(ConstI 5, Cons(ConstI 10, Cons(ConstI 15, Nil(TInt)))) in
  printTerm l;
  let len = refl_trans_typed_beta [] (length l TInt) in
  printTerm len;
  let l = Cons(ConstB false, Cons(ConstB true, Cons(ConstB false, Nil(TBool)))) in
  printTerm l;
  let len = refl_trans_typed_beta [] (length l TBool) in
  printTerm len;
  let l = Cons(ConstI 5, Cons(ConstI 10, Cons(ConstI 15, Cons(ConstI 20, Nil(TInt))))) in
  let ll = refl_trans_typed_beta [] (append l l TInt) in
  printTerm ll;
