open Printf

type pos =
  | XH: pos
  | XO: pos -> pos
  | XI: pos -> pos

(*
pos = { XH, (XO XH), (XI XH), XO (XO XH), XI (XO XH), XO (XI XH) }   
*)

let rec pos2String (p: pos): string =
  match p with
  | XH   -> "1"
  | XO k -> pos2String k ^ "0"
  | XI k -> pos2String k ^ "1"

let printPos (p: pos): unit =
  printf "%s\n" (pos2String p)

let rec pos2Int (p: pos): int =
  match p with
  | XH   -> 1
  | XO k -> 2 * pos2Int k 
  | XI k -> 2 * pos2Int k + 1 

let rec int2PosH (n: int) (m: int): pos =
  if (n >= 1) && (m >= 2) then int2PosH (n-1) (m-2)
  else if (n >= 1) && m = 1 then XO (int2PosH (n-1) (n-1))
  else if (n >= 1) && m = 0 then XI (int2PosH (n-1) (n-1))
  else XH

let int2Pos (i: int): pos = 
  if i < 0 then failwith "not a positive integer"
  else int2PosH i (i+1)

let posEq (n: pos) (m: pos): bool =
  pos2Int n = pos2Int m

(*
let main: unit =
  let p = XI (XO (XI XH)) in
  let i = pos2Int p in
  printPos p;
  printf "%d\n" i;
  let p' = int2Pos i in
  printPos p';
*)