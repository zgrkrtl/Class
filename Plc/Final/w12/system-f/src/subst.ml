open Printf
open Types
open Terms

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
  | Fst e1         -> unique(fvH e1 acc)
  | Snd e1         -> unique(fvH e1 acc)
  | Var x          -> x :: acc
  | Lambda(x,t,e1) -> List.filter (fun a -> a != x) (fvH e1 acc)
  | App(e1,e2)     -> unique(fvH e1 acc @ fvH e2 acc)
  | Plus(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | Minus(e1,e2)   -> unique(fvH e1 acc @ fvH e2 acc)
  | Mult(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | IsEq(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | Leq(e1,e2)     -> unique(fvH e1 acc @ fvH e2 acc)
  | Not e1         -> unique(fvH e1 acc)
  | Ite(e1,e2,e3)  -> unique(fvH e1 acc @ fvH e2 acc @ fvH e3 acc)
  | Fix e1         -> unique(fvH e1 acc)
  | PLambda(x,e1)  -> List.filter (fun a -> a != x) (fvH e1 acc)
  | TApp(e1,t1)    -> unique(fvH e1 acc)
  | Cons(e1,e2)    -> unique(fvH e1 acc @ fvH e2 acc)
  | Head e1        -> unique(fvH e1 acc)
  | Tail e1        -> unique(fvH e1 acc)
  | _              -> acc

let fv(t: term): string list =
  fvH t []

let rec fvTH(t: myType) (acc: string list): string list =
  match t with
  | TVar s        -> s :: acc
  | Forall(x,t1)  -> List.filter (fun a -> a != x) (fvTH t1 acc)
  | TArrow(t1,t2) -> unique(fvTH t1 acc @ fvTH t2 acc)
  | TProd(t1,t2)  -> unique(fvTH t1 acc @ fvTH t2 acc)
  | List t1       -> unique(fvTH t1 acc)
  | _             -> acc 

let fvT(t: myType): string list =
  fvTH t []

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
  | PLambda(y,e1)  -> if x = y then true else inTerm x e1
  | TApp(e1,t1)    -> inTerm x e1
  | Cons(e1,e2)    -> inTerm x e1 || inTerm x e2
  | Head e1        -> inTerm x e1
  | Tail e1        -> inTerm x e1
  | _              -> false

let rec inType(x: string) (t: myType): bool =
  match t with
  | TVar s        -> x = s
  | Forall(s,t1)  -> if x = s then true else inType x t1
  | TArrow(t1,t2) -> inType x t1 || inType x t2
  | TProd(t1,t2)  -> inType x t1 || inType x t2
  | List t1       -> inType x t1
  | _             -> false 


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
  | PLambda(s,e1)  -> if x = s then PLambda(y,replace x y e1) else PLambda(s,replace x y e1) 
  | TApp(e1,t1)    -> TApp(replace x y e1, t1)
  | Cons(e1,e2)    -> Cons(replace x y e1, replace x y e2)
  | Head e1        -> Head(replace x y e1)
  | Tail e1        -> Tail(replace x y e1)
  | _              -> e

let rec replaceType(x: string) (y: string) (t: myType): myType =
  match t with
  | TVar s        -> if x = s then TVar y else t
  | Forall(s,t1)  -> if x = s then Forall(y, replaceType x y t1) else Forall(s, replaceType x y t1)
  | TArrow(t1,t2) -> TArrow(replaceType x y t1, replaceType x y t2)
  | TProd(t1,t2)  -> TProd(replaceType x y t1, replaceType x y t2)
  | List t1       -> List(replaceType x y t1)
  | _             -> t 

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
  | PLambda(y,e1)  -> if y != x && inTerm x e1 = false
                      then PLambda(x,replace y x e1)
                      else e1
  | TApp(e1,t1)    -> TApp(alpha x e1, t1)
  | Cons(e1,e2)    -> Cons(alpha x e1, alpha x e2)
  | Head e1        -> Head(alpha x e1)
  | Tail e1        -> Tail(alpha x e1)
  | _              -> e

let rec alphaType(x: string) (t: myType): myType =
  match t with
  | Forall(y,t1)  -> if y != x && inType x t1 = false
                     then Forall(x,replaceType y x t1)
                     else t1
  | TArrow(t1,t2) -> TArrow(alphaType x t1, alphaType x t2)
  | TProd(t1,t2)  -> TProd(alphaType x t1, alphaType x t2)
  | List t1       -> List(alphaType x t1)
  | _             -> t 

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
  | PLambda(y,e1) -> if y != x && find y (fv s) = false 
                     then PLambda(y,subst e1 x s)
                     else if y = x && find y (fv s) = false 
                     then e
                     else if y = x && find y (fv s) = true 
                     then e
                     else 
                       let z  = freshVar y in
                       let e1' = alpha z e1 in
                       subst (PLambda(z,e1')) x s
  | TApp(e1,t1)   -> TApp(subst e1 x s, t1)
  | Cons(e1,e2)   -> Cons(subst e1 x s, subst e2 x s)
  | Head e1       -> Head(subst e1 x s)
  | Tail e1        -> Tail(subst e1 x s)
  | _             -> e

let rec substType(t: myType) (x: string) (s: myType): myType =
  match t with
  | TVar y        -> if y = x then s else t    
  | Forall(y,t1)  -> if y != x && find y (fvT s) = false 
                     then Forall(y,substType t1 x s)
                     else if y = x && find y (fvT s) = false 
                     then t
                     else if y = x && find y (fvT s) = true 
                     then t
                     else 
                       let z  = freshVar y in
                       let t1' = alphaType z t1 in
                       substType (Forall(z,t1')) x s
  | TArrow(t1,t2) -> TArrow(substType t1 x s, substType t2 x s)
  | TProd(t1,t2)  -> TProd(substType t1 x s, substType t2 x s)
  | List t1       -> List(substType t1 x s)
  | _             -> t 

let rec substTermType(e: term) (x: string) (t: myType): term =
  match e with
  | Pair(e1,e2)     -> Pair(substTermType e1 x t, substTermType e2 x t)
  | Fst e1          -> Fst(substTermType e1 x t)
  | Snd e1          -> Snd(substTermType e1 x t)
  | Var y           -> Var y
  | Lambda(y,t1,e1) -> Lambda(y, substType t1 x t, substTermType e1 x t)
  | App(e1,e2)      -> App(substTermType e1 x t, substTermType e2 x t)
  | Plus(e1,e2)     -> Plus(substTermType e1 x t, substTermType e2 x t)
  | Minus(e1,e2)    -> Minus(substTermType e1 x t, substTermType e2 x t)
  | Mult(e1,e2)     -> Mult(substTermType e1 x t, substTermType e2 x t)
  | IsEq(e1,e2)     -> IsEq(substTermType e1 x t, substTermType e2 x t)
  | Leq(e1,e2)      -> Leq(substTermType e1 x t, substTermType e2 x t)
  | Not e1          -> Not(substTermType e1 x t)
  | Ite(e1,e2,e3)   -> Ite(substTermType e1 x t, substTermType e2 x t, substTermType e3 x t)
  | Fix e1          -> Fix(substTermType e1 x t)
  | PLambda(y,e1)   -> PLambda(y, substTermType e1 x t)
  | TApp(e1,t1)     -> TApp(substTermType e1 x t, substType t1 x t)
  | Cons(e1,e2)     -> Cons(substTermType e1 x t, substTermType e2 x t)
  | Nil t1          -> Nil(substType t1 x t)
  | Head e1         -> Head(substTermType e1 x t)
  | Tail e1         -> Tail(substTermType e1 x t)
  | _               -> e
