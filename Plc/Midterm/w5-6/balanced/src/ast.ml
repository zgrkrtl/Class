open Printf

type balanced = 
  | App   : (balanced*balanced) -> balanced
  | Uni   : balanced            -> balanced
  | Const : balanced
  | Ident : string              -> balanced
  | Num   : int                 -> balanced

let rec balanced2String(b: balanced): string =
  match b with
  | App(b1, b2) -> balanced2String b1 ^ balanced2String b2
  | Uni b1      -> "[" ^ balanced2String b1 ^ "]"
  | Const       -> "[]"
  | Ident s     -> s
  | Num i       -> string_of_int i

let printBalanced(b: balanced): unit =
  printf "%s\n" (balanced2String b)



