open Printf

type 'a myList =
  | Nil : 'a myList
  | Cons: ('a*'a myList) -> 'a myList

type intList = int myList

let rec intList2String (l: intList): string =
  match l with
  | Nil           -> ""
  | Cons (x, Nil) -> string_of_int x
  | Cons (x,xs)   -> string_of_int x ^ ";" ^ intList2String xs

let printIntList (l: intList): unit =
  printf "%s\n" ("[" ^ intList2String l ^ "]")

(******************************************************************)

let rec list2String (l: int list): string =
  match l with
  | []     -> ""
  | [x]   -> string_of_int x
  | x::xs -> string_of_int x ^ ";" ^ list2String xs

let printList (l: int list): unit =
  printf "%s\n" ("[" ^ list2String l ^ "]")

let rec myConcat (l1: 'a list) (l2: 'a list): 'a list =
  match l1 with
  | []    -> l2
  | x::xs -> x :: myConcat xs l2

let rec myMap (f: 'a -> 'b) (l: 'a list): 'b list =
  match l with
  | []    -> []
  | x::xs -> f x :: myMap f xs

let rec myFoldR (f: 'a -> 'b -> 'b) (d: 'b) (l: 'a list): 'b =
  match l with
  | []    -> d
  | x::xs -> f x (myFoldR f d xs)

let main: unit =
  let l1 = [5;3;2] in
  let l2 = [5;3;2;1] in
  let l = myConcat l1 l2 in
  printList l;
  let f = fun x y -> x + y in
  let v = myFoldR f 0 l in
  printf "%d\n" v

(*
Nil          -> []
Cons (x, xs) -> x::xs  
*)

