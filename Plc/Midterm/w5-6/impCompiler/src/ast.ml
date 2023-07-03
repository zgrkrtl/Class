open Printf

type aexpr =
  | Aconst: int    -> aexpr
  | Var   : string -> aexpr
  | Plus  : (aexpr*aexpr) -> aexpr 
  | Minus : (aexpr*aexpr) -> aexpr
  | Mult  : (aexpr*aexpr) -> aexpr
  | Div   : (aexpr*aexpr) -> aexpr

let rec aexpr2String(a: aexpr): string =
  match a with
  | Aconst i      -> string_of_int i
  | Var s         -> "Var " ^ s
  | Plus(a1, a2)  -> aexpr2String a1 ^ " + " ^ aexpr2String a2
  | Minus(a1, a2) -> aexpr2String a1 ^ " - " ^ aexpr2String a2
  | Mult(a1, a2)  -> aexpr2String a1 ^ " * " ^ aexpr2String a2
  | Div(a1, a2)   -> aexpr2String a1 ^ " / " ^ aexpr2String a2 

let printAexpr(a: aexpr): unit =
  printf"%s\n" (aexpr2String a)

type bexpr =
  | Bconst: bool -> bexpr
  | And   : (bexpr*bexpr) -> bexpr
  | Or    : (bexpr*bexpr) -> bexpr
  | Eq    : (aexpr*aexpr) -> bexpr
  | Gt    : (aexpr*aexpr) -> bexpr
  | Lt    : (aexpr*aexpr) -> bexpr
  | Neg   : bexpr         -> bexpr

let rec bexpr2String(b: bexpr): string =
  match b with
  | Bconst b1   -> string_of_bool b1
  | And(b1, b2) -> bexpr2String b1 ^ " && " ^ bexpr2String b2
  | Or(b1, b2)  -> bexpr2String b1 ^ " || " ^ bexpr2String b2
  | Eq(a1, a2)  -> aexpr2String a1 ^ " = " ^ aexpr2String a2
  | Gt(a1, a2)  -> aexpr2String a1 ^ " > " ^ aexpr2String a2
  | Lt(a1, a2)  -> aexpr2String a1 ^ " < " ^ aexpr2String a2
  | Neg b1      -> "~ " ^ bexpr2String b1

let printBexpr(b: bexpr): unit =
  printf"%s\n" (bexpr2String b)

type cmd = 
  | Skip    : cmd
  | Declare : string          -> cmd
  | Assign  : (string*aexpr)  -> cmd
  | Sequence: (cmd*cmd)       -> cmd
  | Ite     : (bexpr*cmd*cmd) -> cmd
  | While   : (bexpr*cmd)     -> cmd
  
let rec cmd2String(c: cmd): string =
  match c with
  | Skip             -> "skip"
  | Declare s        -> "int " ^ s
  | Assign(s, a)     -> s ^ " := " ^ aexpr2String a
  | Sequence(c1, c2) -> cmd2String c1 ^ "; " ^ cmd2String c2 
  | Ite(b, c1, c2)   -> "if(" ^ bexpr2String b ^") then { " ^ cmd2String c1 ^ " } else { " ^ cmd2String c2 ^ " }"
  | While(b, c1)     -> "while(" ^ bexpr2String b ^") {" ^ cmd2String c1 ^ " }"

let printCmd(c: cmd): unit =
  printf "%s\n" (cmd2String c)

