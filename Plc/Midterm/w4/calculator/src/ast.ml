open Printf

type expr =
  | Plus : (expr*expr) -> expr
  | Minus: (expr*expr) -> expr 
  | Mult : (expr*expr) -> expr
  | Div  : (expr*expr) -> expr 
  | Par  : expr        -> expr
  | Num  : int         -> expr
  | Ident: string      -> expr

let rec expr2String(e: expr): string =
  match e with
  | Plus(e1, e2)  -> "[" ^ expr2String e1 ^ " + " ^ expr2String e2 ^ "]"
  | Minus(e1, e2) -> "[" ^ expr2String e1 ^ " - " ^ expr2String e2 ^ "]"
  | Mult(e1, e2)  -> "[" ^ expr2String e1 ^ " * " ^ expr2String e2 ^ "]"
  | Div(e1, e2)   -> "[" ^ expr2String e1 ^ " / " ^ expr2String e2 ^ "]"
  | Par e1        -> "LPAREN " ^ expr2String e1 ^ " RPAREN"
  | Num i         -> string_of_int i
  | Ident s       -> s  

let printExpr(e: expr): unit =
  printf "%s\n" (expr2String e)

let rec eval(e: expr): int =
  match e with
  | Plus(e1, e2)  -> eval e1 + eval e2
  | Minus(e1, e2) -> eval e1 - eval e2
  | Mult(e1, e2)  -> eval e1 * eval e2
  | Div(e1, e2)   -> eval e1 / eval e2
  | Par e1        -> eval e1
  | Num i         -> i
  | Ident s       -> failwith "string"  

