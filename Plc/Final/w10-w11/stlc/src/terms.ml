open Printf
open Types

type term = 
  | ConstI : int                  -> term (* ConstI 5 *)
  | ConstB : bool                 -> term (* ConstB true *)
  | Pair   : (term*term)          -> term
  | Fst    : term                 -> term
  | Snd    : term                 -> term
  | Var    : string               -> term
  | Lambda : (string*myType*term) -> term
  | App    : (term*term)          -> term
  | Plus   : (term*term)          -> term
  | Minus  : (term*term)          -> term
  | Mult   : (term*term)          -> term
  | IsEq   : (term*term)          -> term
  | Leq    : (term*term)          -> term
  | Not    : term                 -> term
  | Ite    : (term*term*term)     -> term
  | Fix    : term                 -> term


let rec term2String(e: term): string =
  match e with
  | ConstI i       -> string_of_int i
  | ConstB b       -> string_of_bool b 
  | Pair(e1,e2)    -> "(" ^ term2String e1 ^ "," ^ term2String e2 ^ ")" (* (x,y) *)
  | Fst e1         -> "(pi1" ^ term2String e1 ^ ")" 
  | Snd e1         -> "(pi2" ^ term2String e1 ^ ")" 
  | Var s          -> s
  | Lambda(y,t,e1) -> "(λ" ^ y ^ ": " ^ myType2String t ^ ". " ^ term2String e1 ^ ")"
  | App(e1, e2)    -> "[" ^ term2String e1 ^ " " ^ term2String e2 ^ "]"
  | Plus(e1, e2)   -> "(" ^ term2String e1 ^ " + " ^ term2String e2 ^ ")"
  | Minus(e1, e2)  -> "(" ^ term2String e1 ^ " - " ^ term2String e2 ^ ")"
  | Mult(e1, e2)   -> "(" ^ term2String e1 ^ " * " ^ term2String e2 ^ ")"
  | IsEq(e1, e2)   -> "(" ^ term2String e1 ^ " = " ^ term2String e2 ^ ")"
  | Leq(e1, e2)    -> "(" ^ term2String e1 ^ " <= " ^ term2String e2 ^ ")"
  | Not e1         -> "~(" ^ term2String e ^ ")" 
  | Ite(e1,e2,e3)  -> "if " ^ term2String e1 ^ " then { " ^ term2String e2 ^ " } else { " ^ term2String e3 ^ " }"
  | Fix e1         -> "fix " ^ term2String e1


let printTerm(e: term): unit =
  printf "%s\n" (term2String e)


