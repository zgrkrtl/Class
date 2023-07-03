open Printf
open Types
open Terms
open Typecheck

let rec find(y: string) (l: string list): bool =
  match l with
  | []    -> false
  | x::xs -> if x = y then true else find y xs

let rec uniqueH(l: string list) (acc: string list): string list =
  match l with
  | []    -> acc
  | x::xs -> if find x acc then uniqueH xs acc else uniqueH xs (acc @ [x])

let unique(l: string list): string list =
  uniqueH l []

let rec list2String(l: string list): string =
  match l with
  | []    -> ""
  | [x]   -> x
  | x::xs -> x ^ "," ^ list2String xs

let printList(l: string list): unit =
  printf("[%s]\n") (list2String l)

let rec fvH(e: term) (acc: string list): string list =
  match e with
  | Pair(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | Fst e1         -> fvH e1 acc
  | Snd e1         -> fvH e1 acc
  | Var x          -> x :: acc
  | Lambda(x,t,e1) -> List.filter (fun a -> a != x) (fvH e1 acc)
  | App(e1,e2)     -> unique(fvH e1 acc @ fvH e2 acc)
  | Plus(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | Minus(e1,e2)   -> unique(fvH e1 acc @ fvH e2 acc)
  | Mult(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | IsEq(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | Leq(e1,e2)     -> unique(fvH e1 acc @ fvH e2 acc)
  | Not e1         -> fvH e1 acc
  | Ite(e1,e2,e3)  -> unique(fvH e1 acc @ fvH e2 acc @ fvH e3 acc)
  | Fix e1         -> fvH e1 acc
  | _              -> acc

let fv(t: term): string list =
  fvH t []

let rec inTerm(x: string) (e: term): bool =
  match e with
  | Pair(e1,e2)    -> inTerm x e1 || inTerm x e2
  | Fst e1         -> inTerm x e1
  | Snd e1         -> inTerm x e1
  | Var y          -> x = y
  | Lambda(y,t,e1) -> if x = y then true else inTerm x e1
  | App(e1,e2)     -> inTerm x e1 || inTerm x e2
  | Plus(e1,e2)    -> inTerm x e1 || inTerm x e2
  | Minus(e1,e2)   -> inTerm x e1 || inTerm x e2
  | Mult(e1,e2)    -> inTerm x e1 || inTerm x e2
  | IsEq(e1,e2)    -> inTerm x e1 || inTerm x e2
  | Leq(e1,e2)     -> inTerm x e1 || inTerm x e2
  | Not e1         -> inTerm x e1
  | Ite(e1,e2,e3)  -> inTerm x e1 || inTerm x e2 || inTerm x e3
  | Fix e1         -> inTerm x e1
  | _              -> false



let rec replace(x: string) (y: string) (e: term): term =
  match e with
  | Pair(e1, e2)   -> Pair(replace x y e1, replace x y e2)
  | Fst e1         -> Fst(replace x y e1)
  | Snd e1         -> Snd(replace x y e1)
  | Var s          -> if x = s then Var y else e
  | Lambda(s,t,e1) -> if x = s then Lambda(y,t,replace x y e1) else Lambda(s,t,replace x y e1) 
  | App(e1, e2)    -> App(replace x y e1, replace x y e2)
  | Plus(e1, e2)   -> Plus(replace x y e1, replace x y e2)
  | Minus(e1, e2)  -> Minus(replace x y e1, replace x y e2)
  | Mult(e1, e2)   -> Mult(replace x y e1, replace x y e2)
  | IsEq(e1, e2)   -> IsEq(replace x y e1, replace x y e2)
  | Leq(e1, e2)    -> Leq(replace x y e1, replace x y e2)
  | Not e1         -> Not(replace x y e1)
  | Ite(e1,e2,e3)  -> Ite(replace x y e1, replace x y e2, replace x y e3)
  | Fix e1         -> Fix(replace x y e1)
  | _              -> e



let rec alpha(x: string) (e: term): term =
  match e with
  | Pair(e1,e2)    -> Pair(alpha x e1, alpha x e2)
  | Fst e1         -> Fst(alpha x e1)
  | Snd e1         -> Snd(alpha x e1)
  | Var y          -> Var y
  | Lambda(y,t,e1) -> if y != x && inTerm x e1 = false
                      then Lambda(x,t,replace y x e1)
                      else e1
  | App(e1,e2)     -> App(alpha x e1, alpha x e2)
  | Plus(e1,e2)    -> Plus(alpha x e1, alpha x e2)
  | Minus(e1,e2)   -> Minus(alpha x e1, alpha x e2)
  | Mult(e1,e2)    -> Mult(alpha x e1, alpha x e2)
  | IsEq(e1,e2)    -> IsEq(alpha x e1, alpha x e2)
  | Leq(e1,e2)     -> Leq(alpha x e1, alpha x e2)
  | Not e1         -> Not(alpha x e1)
  | Ite(e1,e2,e3)  -> Ite(alpha x e1, alpha x e2, alpha x e3)
  | Fix e1         -> Fix(alpha x e1)
  | _              -> e



let x = ref 0

let freshVar(s: string): string =
  x := !x + 1; s ^ (string_of_int !x)
  
let rec subst(e: term) (x: string) (s: term): term =
  match e with
  | Pair(e1,e2)    -> Pair(subst e1 x s, subst e2 x s)
  | Fst e1         -> Fst(subst e1 x s)
  | Snd e1         -> Snd(subst e1 x s)
  | Var y          -> if x = y then s else e 
  | Lambda(y,t,e1) -> if y != x && find y (fv s) = false 
                      then Lambda(y,t,subst e1 x s)
                      else if y = x && find y (fv s) = false 
                      then e
                      else if y = x && find y (fv s) = true 
                      then e
                      else 
                        let z  = freshVar y in
                        let e1' = alpha z e1 in
                        subst (Lambda(z,t,e1')) x s
  | App(e1,e2)    -> App(subst e1 x s, subst e2 x s)
  | Plus(e1,e2)   -> Plus(subst e1 x s, subst e2 x s)
  | Minus(e1,e2)  -> Minus(subst e1 x s, subst e2 x s)
  | Mult(e1,e2)   -> Mult(subst e1 x s, subst e2 x s)
  | IsEq(e1,e2)   -> IsEq(subst e1 x s, subst e2 x s)
  | Leq(e1,e2)    -> Leq(subst e1 x s, subst e2 x s)
  | Not e1        -> Not(subst e1 x s)
  | Ite(e1,e2,e3) -> Ite(subst e1 x s, subst e2 x s, subst e3 x s)
  | Fix e1        -> Fix(subst e1 x s)
  | _             -> e