open Printf

type bit =
  | Z
  | O

let printBit (b: bit): unit =
  match b with
  | Z -> printf "0"
  | O -> printf "1"

type binNum =
  | Bit  of bit
  | Join of (bit * binNum)
  | Pos  of binNum
  | Neg  of binNum

let rec printBinNum (n: binNum): unit =
  match n with
  | Bit b       -> printBit b
  | Join (b, x) -> printBinNum x; printBit b
  | Pos x       -> printBinNum x
  | Neg x       -> printf "-"; printBinNum x

let rec toBinNum (n: int): binNum =
  if n < 0 then Neg (toBinNum(-n))
  else if n = 0 then Bit Z
  else if n = 1 then Bit O
  else if (n mod 2 = 0) then Join (Z, toBinNum (n/2))
  else Join (O, toBinNum ((n-1)/2))

let main: unit =
  let b = Z in printBit b; printf "\n";
  let b = Join (O, Bit Z) in printBinNum b; printf "\n";
  let b = toBinNum (10) in printBinNum b; printf "\n";


(* ocamlc binNum.ml -o binNum *)
(* ./binNum *)