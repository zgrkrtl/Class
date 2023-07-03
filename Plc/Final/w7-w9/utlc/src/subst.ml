open Printf
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

let rec fvH(t: term) (acc: string list): string list =
  match t with
  | Var x        -> x :: acc
  | Lambda(x, e) -> List.filter (fun a -> a != x) (fvH e acc)
  | App(e1, e2)  -> unique(fvH e1 acc @ fvH e2 acc)

let fv(t: term): string list =
  fvH t []

let rec inTerm(x: string) (t: term): bool =
  match t with
  | Var y         -> x = y
  | Lambda(y, t1) -> if x = y then true else inTerm x t1
  | App(t1, t2)   -> inTerm x t1 || inTerm x t2

let rec replace(x: string) (y: string) (t: term): term =
  match t with
  | Var s         -> if x = s then Var y else t
  | Lambda(s, t1) -> if x = s then Lambda(y, replace x y t1) else Lambda(s, replace x y t1) 
  | App(t1, t2)   -> App(replace x y t1, replace x y t2)

let rec alpha(x: string) (t: term): term =
  match t with
  | Var y         -> Var y
  | Lambda(y, t1) -> if y != x && inTerm x t1 = false
                     then Lambda(x, replace y x t1)
                     else t
  | App(t1, t2)   -> App(alpha x t1, alpha x t2)

let x = ref 0

let freshVar(s: string): string =
  x := !x + 1; s ^ (string_of_int !x)
  
let rec subst(t: term) (x: string) (s: term): term =
  match t with  
  | Var y         -> if x = y then s else t
  | Lambda(y, t1) -> if y != x && find y (fv s) = false 
                      then Lambda(y, subst t1 x s)
                      else if y = x && find y (fv s) = false 
                      then t
                      else if y = x && find y (fv s) = true 
                      then t
                      else 
                        let z  = freshVar y in
                        let t' = alpha z t in
                        subst t' x s
  | App(e1, e2)   -> App(subst e1 x s, subst e2 x s)