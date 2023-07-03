open Printf

type llist = 
  | Nil : llist
  | Cons: (int*llist) -> llist

let rec llist2String(l: llist): string =
  match l with
    | Nil          -> ""
    | Cons(i, Nil) -> string_of_int i
    | Cons(i, l1)  -> string_of_int i ^ "," ^ llist2String l1

let printLlist(l: llist): unit =
  printf "[%s]\n" (llist2String l)

let rec length(l: llist): int =
  match l with
  | Nil         -> 0
  | Cons(i, l1) -> length l1 + 1

let rec append(l: llist) (m: llist): llist =
  match l with
  | Nil         -> m
  | Cons(x, xs) -> Cons(x, append xs m)

let rec reverse(l: llist): llist =
  match l with
  | Nil          -> Nil
  | Cons (x, xs) -> append (reverse xs) (Cons(x, Nil))