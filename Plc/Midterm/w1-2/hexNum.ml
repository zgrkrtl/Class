open Printf

type hex =
  | Zero  | One  | Two  | Three  | Four  | Five  | Six  | Seven  | Eight  | Nine
  | A     | B    | C    | D      | E     | F
 
let rec hex2String (h: hex): string =
  match h with
    | Zero  -> "0" 
    | One   -> "1"
    | Two   -> "2"
    | Three -> "3"
    | Four  -> "4"
    | Five  -> "5"
    | Six   -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine  -> "9"
    | A     -> "A"
    | B     -> "B"
    | C     -> "C"
    | D     -> "D"
    | E     -> "E"
    | F     -> "F"

let rec printHex (h: hex): unit =
  printf "%s\n" (hex2String h)

type hexNum =
  | Hex  : hex            -> hexNum
  | JoinH: (hex * hexNum) -> hexNum
  | NegH : hexNum         -> hexNum
  | PosH : hexNum         -> hexNum

let rec hexNum2String (h: hexNum): string =
  match h with
    | Hex a        -> hex2String a
    | JoinH (a, b) -> hexNum2String b ^ hex2String a
    | NegH a       -> "-" ^ hexNum2String a
    | PosH a       -> hexNum2String a

let rec printHexNum (h: hexNum): unit =
  printf "%s\n" (hexNum2String h)

let rec int2Hex (n: int): hexNum =
  if n < 0 then NegH(int2Hex(-n))
  else if n = 0 then (Hex Zero)
  else if n = 1 then (Hex One)
  else if n = 2 then (Hex Two)
  else if n = 3 then (Hex Three)
  else if n = 4 then (Hex Four)
  else if n = 5 then (Hex Five)
  else if n = 6 then (Hex Six)
  else if n = 7 then (Hex Seven)
  else if n = 8 then (Hex Eight)
  else if n = 9 then (Hex Nine)
  else if n = 10 then (Hex A)
  else if n = 11 then (Hex B)
  else if n = 12 then (Hex C)
  else if n = 13 then (Hex D)
  else if n = 14 then (Hex E)
  else if n = 15 then (Hex F)
  else if (n mod 16 = 0)  then JoinH (Zero, int2Hex (n/16))
  else if (n mod 16 = 1)  then JoinH (One, int2Hex ((n-1)/16))
  else if (n mod 16 = 2)  then JoinH (Two, int2Hex ((n-2)/16))
  else if (n mod 16 = 3)  then JoinH (Three, int2Hex ((n-3)/16))
  else if (n mod 16 = 4)  then JoinH (Four, int2Hex ((n-4)/16))
  else if (n mod 16 = 5)  then JoinH (Five, int2Hex ((n-5)/16))
  else if (n mod 16 = 6)  then JoinH (Six, int2Hex ((n-6)/16))
  else if (n mod 16 = 7)  then JoinH (Seven, int2Hex ((n-7)/16))
  else if (n mod 16 = 8)  then JoinH (Eight, int2Hex ((n-8)/16))
  else if (n mod 16 = 9)  then JoinH (Nine, int2Hex ((n-9)/16))
  else if (n mod 16 = 10) then JoinH (A, int2Hex ((n-10)/16))
  else if (n mod 16 = 11) then JoinH (B, int2Hex ((n-11)/16))
  else if (n mod 16 = 12) then JoinH (C, int2Hex ((n-12)/16))
  else if (n mod 16 = 13) then JoinH (D, int2Hex ((n-13)/16))
  else if (n mod 16 = 14) then JoinH (E, int2Hex ((n-14)/16))
  else JoinH (F, int2Hex ((n-15)/16))

let rec hex2Int (h: hexNum): int =
  match h with
    | Hex a -> if a = Zero then 0 
               else if a = One then 1
               else if a = Two then 2
               else if a = Three then 3
               else if a = Four then 4
               else if a = Five then 5
               else if a = Six then 6
               else if a = Seven then 7
               else if a = Eight then 8
               else if a = Nine then 9
               else if a = A then 10
               else if a = B then 11
               else if a = C then 12
               else if a = D then 13
               else if a = E then 14
               else 15
    | JoinH (a, b) -> if a = Zero then (16 * hex2Int b) 
                      else if a = One then (16 * hex2Int b + 1) 
                      else if a = Two then (16 * hex2Int b + 2)
                      else if a = Three then (16 * hex2Int b + 3) 
                      else if a = Four then (16 * hex2Int b + 4) 
                      else if a = Five then (16 * hex2Int b + 5) 
                      else if a = Six then (16 * hex2Int b + 6) 
                      else if a = Seven then (16 * hex2Int b + 7) 
                      else if a = Eight then (16 * hex2Int b + 8) 
                      else if a = Nine then (16 * hex2Int b + 9) 
                      else if a = A then (16 * hex2Int b + 10) 
                      else if a = B then (16 * hex2Int b + 11) 
                      else if a = C then (16 * hex2Int b + 12) 
                      else if a = D then (16 * hex2Int b + 13) 
                      else if a = E then (16 * hex2Int b + 14) 
                      else (16 * hex2Int b + 15) 
    | NegH a -> -(hex2Int a)
    | PosH a -> hex2Int a

let main: unit =
  let h = int2Hex (-18) in printHexNum h; 
  let h = int2Hex 32 in printHexNum h;
  let i = hex2Int (int2Hex 12000) in print_int i; 
  printf"\n";
  let i = hex2Int (int2Hex (-100)) in print_int i; 
  printf"\n";
  printHexNum (Hex A); 
  printHexNum (JoinH(A, JoinH(D, Hex Seven)));