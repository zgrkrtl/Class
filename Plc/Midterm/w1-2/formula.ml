open Printf

type formula =
  | True : formula
  | False: formula
  | And  : (formula * formula) -> formula
  | Or   : (formula * formula) -> formula

(*
formula = { True, False, And (True, True), And (False, True), And (True, False), And (False, False),
            And (And(True, True), True), And (And (And (True, False), False), True), 
            Or (True, False), Or (And (True, True), True), etc... }   
*)

let rec formula2String (f: formula): string =
  match f with
  | True         -> "1"
  | False        -> "0"
  | And (f1, f2) -> "(" ^ formula2String f1 ^ " && " ^ formula2String f2 ^ ")" 
  | Or (f1, f2)  -> "(" ^ formula2String f1 ^ " || " ^ formula2String f2 ^ ")"

let printFormula (f: formula): unit =
  printf "%s\n" (formula2String f)

let rec eval (f: formula): formula =
  match f with
  | True              -> True
  | False             -> False
  | And (False, _)    -> False
  | And (_, False)    -> False
  | And (True, True)  -> True
  | Or (True, _)      -> True
  | Or (_, True)      -> True
  | Or (False, False) -> False
  | And (f1, f2)      -> And (eval f1, eval f2)
  | Or  (f1, f2)      -> Or (eval f1, eval f2)

let rec multiStep (f: formula): formula =
  if f = evalf  then f else multiStep (eval f)

let main: unit =
  let f = And(And(And(True, False), True), True) in
  printFormula f; 
  let f' = multiStep f in
  printFormula f';
