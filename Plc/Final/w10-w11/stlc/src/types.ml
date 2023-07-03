open Printf

type myType = 
  | TInt  : myType
  | TBool : myType
  | TVar  : string -> myType
  | TProd : (myType*myType) -> myType
  | TArrow: (myType*myType) -> myType

let rec myType2String(t: myType): string =
  match t with
  | TInt          -> "Z"
  | TBool         -> "B"
  | TVar s        -> s
  | TProd(t1,t2)  -> "(" ^ myType2String t1 ^ " x " ^ myType2String t2 ^ ")"
  | TArrow(t1,t2) -> "(" ^ myType2String t1 ^ " -> " ^ myType2String t2 ^ ")"

let printMyType(t: myType): unit =
  printf "%s\n" (myType2String t)