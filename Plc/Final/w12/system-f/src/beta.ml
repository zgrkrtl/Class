open Printf
open Types
open Terms
open Subst
open Typecheck

let rec beta(e: term): term =
  match e with
  | Fst(Pair(e1,e2))        -> e1
  | Snd(Pair(e1,e2))        -> e2
  | Pair(e1,e2)             -> Pair(beta e1, beta e2)
  | App(Lambda(y,t,e1), e2) -> subst e1 y e2
  | App(e1, e2)             -> App(beta e1, beta e2)
  | Lambda(y,t,e1)          -> Lambda(y,t, beta e1)
  | Plus(e1,e2)             -> (
                                  match (e1,e2) with
                                  | (ConstI n, ConstI m) -> ConstI(n+m)
                                  | _                    -> Plus(beta e1, beta e2)
                               )
  | Minus(e1,e2)            -> (
                                  match (e1,e2) with
                                  | (ConstI n, ConstI m) -> ConstI(n-m)
                                  | _                    -> Minus(beta e1, beta e2)
                                )
  | Mult(e1,e2)             ->  (
                                  match (e1,e2) with
                                  | (ConstI n, ConstI m) -> ConstI(n*m)
                                  | _                    -> Mult(beta e1, beta e2)
                                )
  | IsEq(e1,e2)            -> (
                                  match (e1,e2) with
                                  | (ConstI n, ConstI m) -> ConstB(n=m)
                                  | (ConstB n, ConstB m) -> ConstB(n=m)
                                  | (Nil t1, Nil t2)     -> ConstB(t1=t2)
                                  | (Nil t1, _)          -> ConstB false
                                  | (_, Nil t1)          -> ConstB false
                                  | (Cons(e1,e2), Cons(e3,e4)) -> if e1 = e3 then IsEq(e2,e4) else ConstB false
                                  | _                    -> IsEq(beta e1, beta e2)
                                )
  | Leq(e1,e2)              ->  (
                                  match (e1,e2) with
                                  | (ConstI n, ConstI m) -> ConstB(n<=m)
                                  | _                    -> Leq(beta e1, beta e2)
                                )
  | Not e1                  -> (
                                  match e1 with
                                  | ConstB false -> ConstB true
                                  | ConstB true  -> ConstB false
                                  | _            -> Not(beta e1)
                               )
  | Ite(ConstB false,e2,e3) -> e3
  | Ite(ConstB true,e2,e3)  -> e2
  | Ite(e1,e2,e3)           -> Ite(beta e1,e2,e3)
  | Fix(Lambda(x,t,e1))     -> subst e1 x (Fix(Lambda(x,t,e1)))
  | Fix e1                  -> Fix (beta e1)
  | TApp(PLambda(y,e1),t1)  -> substTermType e1 y t1
  | TApp(e1,t1)             -> TApp(beta e1, t1)
  | Head(Cons(e1,e2))       -> e1
  | Tail(Cons(e1,e2))       -> e2
  | Cons(e1,e2)             -> Cons(beta e1, beta e2)
  | _                       -> e

let rec refl_trans_beta(t: term): term =
  let t' = beta t in
  if t = t' then t else refl_trans_beta t'

let rec refl_trans_typed_beta(m: ctx) (e: term): term =
  let t = typecheck m e in
  match t with
  | Yes t' -> refl_trans_beta e
  | No s   -> failwith s


