open Printf


type term = 
  | Var   : string        -> term
  | Lambda: (string*term) -> term
  | App   : (term*term)   -> term


let rec term2String(t: term): string =
  match t with
  | Var s         -> s
  | Lambda(y, t1) -> "(λ" ^ y ^ ". " ^ term2String t1 ^ ")"
  | App(t1, t2)   -> "[" ^ term2String t1 ^ " " ^ term2String t2 ^ "]"


let printTerm(t: term): unit =
  printf "%s\n" (term2String t)


