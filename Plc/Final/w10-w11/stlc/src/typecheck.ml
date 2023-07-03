open Printf
open Types
open Terms


type 'a err =
  | Yes: 'a     -> 'a err
  | No : string -> 'a err

type ctx = (string*myType) list

let extend(m: ctx) (s: string) (t: myType): ctx =
  (s,t) :: m

let rec lookup(m: ctx) (s: string): myType err =
  match m with
  | []            -> No "no such variable in the context"
  | (s',t') :: xs -> if s = s' then Yes t' else lookup xs s

let rec typecheck(m: ctx) (e: term): myType err =
  match e with
  | ConstI i        -> Yes TInt
  | ConstB b        -> Yes TBool
  | Pair(e1,e2)     -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       (
                        match (te1,te2) with
                        | (Yes t1, Yes t2) -> Yes(TProd(t1,t2))
                        | _                -> No "ill-typing in pair"
                       )
  | Fst e1          -> let te1 = typecheck m e1 in
                       (
                        match te1 with
                        | Yes(TProd(t1,t2)) -> Yes t1
                        | _                 -> No "ill-typing in fst"
                       )
  | Snd e1          -> let te1 = typecheck m e1 in
                       (
                        match te1 with
                        | Yes(TProd(t1,t2)) -> Yes t2
                        | _                 -> No "ill-typing in snd"
                       )
  | Var s           -> lookup m s
  | Lambda(x,a,t)  -> let m' = extend m x a in
                      let te1 = typecheck m' t in
                       (
                        match te1 with
                        | Yes b -> Yes(TArrow(a,b))
                        | _      -> No "ill-typing in lambda"
                       )
  | App(s,t)        -> let te1 = typecheck m s in
                       let te2 = typecheck m t in
                       (
                        match (te1,te2) with
                        | (Yes(TArrow(a,b)), Yes c) when a = c -> Yes b
                        | _                                    -> No "ill-typing in app" 
                       )
  | Plus(e1,e2)     -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       (
                       match (te1,te2) with
                       | (Yes TInt, Yes TInt) -> Yes TInt
                       | _                    -> No "ill-typing in plus" 
                       )
  | Minus(e1,e2)    -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       (
                       match (te1,te2) with
                       | (Yes TInt, Yes TInt) -> Yes TInt
                       | _                    -> No "ill-typing in minus" 
                       )
  | Mult(e1,e2)     -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       (
                       match (te1,te2) with
                       | (Yes TInt, Yes TInt) -> Yes TInt
                       | _                    -> No "ill-typing in mult" 
                       )
  | IsEq(e1,e2)     -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       (
                       match (te1,te2) with
                       | (Yes TInt, Yes TInt)   -> Yes TBool
                       | (Yes TBool, Yes TBool) -> Yes TBool
                       | _                      -> No "ill-typing in iseq" 
                       )
  | Leq(e1,e2)      -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       (
                       match (te1,te2) with
                       | (Yes TInt, Yes TInt)   -> Yes TBool
                       | _                      -> No "ill-typing in leq" 
                       )
  | Not e1          -> let te1 = typecheck m e1 in
                       (
                        match te1 with
                        | Yes TBool -> Yes TBool
                        | _         -> No "ill-typing in not"
                       )
  | Ite(e1,e2,e3)   -> let te1 = typecheck m e1 in
                       let te2 = typecheck m e2 in
                       let te3 = typecheck m e3 in
                       (
                        match (te1,te2,te3) with
                        | (Yes TBool, Yes t2, Yes t3) when t2 = t3 -> Yes t3
                        | _                                        -> No "ill-typing in ite" 
                       )
  | Fix e1          -> let te1 = typecheck m e1 in
                       (
                        match te1 with
                        | Yes(TArrow(t1,t2)) when t1 = t2 -> Yes t1
                        | _                               -> No "ill-typing in fix"
                       )