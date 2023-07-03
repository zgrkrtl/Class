open Printf
open Ast

type state = (string*int) list

let rec state2String(m: state): string =
  match m with
  | []           -> ""
  | [(x, y)]     -> x ^ ":" ^ string_of_int y
  | (x, y) :: xs -> x ^ ":" ^ string_of_int y ^ " | " ^ state2String xs

let printState(m: state): unit =
  printf "[| %s |]\n" (state2String m)

let extend(m: state) (s: string) (v: int): state = (s, v) :: m

let rec update(m: state) (s: string) (v: int): state =
  match m with
  | []           -> extend m s 0
  | (x, y) :: xs -> if x = s then (x, v) :: xs else (x, y) :: update xs s v

let rec lookup(m: state) (s: string): int =
  match m with
  | []           -> failwith "no such variable in the state\n"
  | (x, y) :: xs -> if x = s then y else lookup xs s
  
let rec evalAexpr(a: aexpr) (m: state): aexpr =
  match a with
  | Aconst i      -> Aconst i
  | Var s         -> Aconst(lookup m s)
  | Plus(a1, a2)  -> let ae1 = evalAexpr a1 m in
                     let ae2 = evalAexpr a2 m in
                     (
                       match (ae1, ae2) with
                       | (Aconst n, Aconst m) -> Aconst(n + m)
                       | _                    -> Plus(ae1, ae2)
                    )
  | Minus(a1, a2) -> let ae1 = evalAexpr a1 m in
                     let ae2 = evalAexpr a2 m in
                     (
                       match (ae1, ae2) with
                       | (Aconst n, Aconst m) -> Aconst(n - m)
                       | _                    -> Minus(ae1, ae2)
                     )
   | Mult(a1, a2) -> let ae1 = evalAexpr a1 m in
                     let ae2 = evalAexpr a2 m in
                     (
                       match (ae1, ae2) with
                       | (Aconst n, Aconst m) -> Aconst(n * m)
                       | _                    -> Mult(ae1, ae2)
                     )
   | Div(a1, a2)  -> let ae1 = evalAexpr a1 m in
                     let ae2 = evalAexpr a2 m in
                     (
                       match (ae1, ae2) with
                       | (Aconst n, Aconst m) -> Aconst(n / m)
                       | _                    -> Div(ae1, ae2)
                     )

let rec evalBexpr(b: bexpr) (m: state): bexpr =
  match b with
  | Bconst b    -> Bconst b
  | And(b1, b2) -> let eb1 = evalBexpr b1 m in
                   let eb2 = evalBexpr b2 m in
                   (
                    match (eb1, eb2) with
                    | (Bconst n, Bconst m) -> Bconst(n && m)
                    | _                    -> And(eb1, eb2)
                   )
  | Or(b1, b2)  -> let eb1 = evalBexpr b1 m in
                   let eb2 = evalBexpr b2 m in
                   (
                    match (eb1, eb2) with
                    | (Bconst n, Bconst m) -> Bconst(n || m)
                    | _                    -> Or(eb1, eb2)
                   )
  | Eq(a1, a2)  -> let ea1 = evalAexpr a1 m in
                   let ea2 = evalAexpr a2 m in
                   (
                    match (ea1, ea2) with
                    | (Aconst n, Aconst m) -> Bconst(n = m)
                    | _                    -> Eq(ea1, ea2)
                   )
  | Gt(a1, a2)  -> let ea1 = evalAexpr a1 m in
                   let ea2 = evalAexpr a2 m in
                   (
                    match (ea1, ea2) with
                    | (Aconst n, Aconst m) -> Bconst(n > m)
                    | _                    -> Gt(ea1, ea2)
                   )
  | Lt(a1, a2)  -> let ea1 = evalAexpr a1 m in
                   let ea2 = evalAexpr a2 m in
                   (
                    match (ea1, ea2) with
                    | (Aconst n, Aconst m) -> Bconst(n < m)
                    | _                    -> Lt(ea1, ea2)
                   )
  | Neg b1       -> let eb1 = evalBexpr b1 m in
                    Neg eb1

let rec evalCmd(c: cmd) (m: state): state =
  match c with
  | Skip         -> m
  | Declare s    -> update m s 0
  | Assign(s, a) -> let ea = evalAexpr a m in
                    (
                      match ea with
                      | Aconst v -> update m s v
                      | _        -> failwith "error in assign" 
                    )
  | Sequence(c1, c2) -> let m' = evalCmd c1 m in evalCmd c2 m'
  | Ite(b, c1, c2)   -> let eb = evalBexpr b m in
                        (
                          match eb with
                          | Bconst b' -> if b' then evalCmd c1 m else evalCmd c2 m
                          | _           -> failwith "error in ite"
                        )
  | While(b, c1)     -> let eb = evalBexpr b m in
                        (
                          match eb with
                          | Bconst b' -> if b' then evalCmd (Sequence(c1, (While(b, c1)))) m else m
                          | _         -> failwith "error in while"
                        )
