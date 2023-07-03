open Printf
open Positive

type rat = 
{
  num  : int;
  denom: pos;
}

let rat2String (r: rat): string =
  string_of_int (r.num) ^ "/" ^ string_of_int (pos2Int (r.denom))

let printRat (r: rat): unit =
  printf "%s\n" (rat2String r)

let ratEq (r1: rat) (r2: rat): bool =
  r1.num * pos2Int (r2.denom) = r2.num * pos2Int (r1.denom)

let rec gcd (a: int) (b: int): int =
  if b = 0 then a else gcd b (a mod b)

(*
|a * b| = gcd a b * lcm a b
*)

let lcm (a: int) (b: int): int =
  match (a,b) with
  | (0, _) -> 0
  | (_, 0) -> 0
  | (n, m) -> abs (n * m) / gcd n m 

let ratAdd (r1: rat) (r2: rat): rat =
  let l = lcm (pos2Int (r1.denom)) (pos2Int (r2.denom)) in
  let n1 = r1.num * (l/(pos2Int (r1.denom))) in
  let n2 = r2.num * (l/(pos2Int (r2.denom))) in
  let r = { num = n1 + n2; denom = int2Pos l } in
  r

let ratSubtr (r1: rat) (r2: rat): rat =
  let l = lcm (pos2Int (r1.denom)) (pos2Int (r2.denom)) in
  let n1 = r1.num * (l/(pos2Int (r1.denom))) in
  let n2 = r2.num * (l/(pos2Int (r2.denom))) in
  let r = { num = n1 - n2; denom = int2Pos l } in
  r

let main: unit =
  let r1 = { num = 5; denom = int2Pos 10 } in
  let r2 = { num = 1; denom = int2Pos 2 } in
  printRat r1;
  printRat r2;
  let b = ratEq r1 r2 in
  printf "%b\n" b;
  let r = ratSubtr r1 r2 in
  printRat r;


