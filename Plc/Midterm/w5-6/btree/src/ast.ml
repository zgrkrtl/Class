open Printf

type btree =
  | Empty: btree
  | Node : (int*btree*btree) -> btree

let rec btree2String(t: btree): string =
  match t with
  | Empty         -> "_"
  | Node(i, l, r) -> "[" ^ string_of_int i ^ "," ^ btree2String l ^ "," ^ btree2String r ^ "]"

let printBtree(t: btree): unit =
  printf"%s\n" (btree2String t)

let rec flatten(t: btree): int list =
  match t with
  | Empty         -> []
  | Node(i, l, r) -> flatten l @ [i] @ flatten r 

let rec intList2String(l: int list): string =
  match l with
  | []    -> ""
  | [x]   -> string_of_int x
  | x::xs -> string_of_int x ^ "," ^ intList2String xs

let printIntList(l: int list): unit =
  printf "[%s]\n" (intList2String l)

let rec size(t: btree): int =
  match t with
  | Empty         -> 0
  | Node(i, l, r) -> size l + size r + 1

let rec heigth(t: btree): int =
  match t with
  | Empty         -> 0
  | Node(i, l, r) -> max (heigth l) (heigth r) + 1