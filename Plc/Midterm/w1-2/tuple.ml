
open Printf

let myCurry (f: ('a*'b) -> 'c): 'a -> 'b -> 'c = fun (x: 'a) -> fun (y: 'b) -> f (x, y)

let addP ((x,y): (int*int)): int = x + y

let myUnCurry (f: 'a -> 'b -> 'c): ('a*'b) -> 'c =
  fun (x: (int*int)) -> f (fst x) (snd x)

let add (x: int) (y: int): int = x + y

let main: unit =
  let v = addP (10, 20) in (* addP (10,20) --> SF 10 20 *)
  printf "%d\n" v;
  let v = (myCurry addP) 10 20 in
  printf "%d\n" v;  
  let v = add 10 20 in (* add 10 20 --> SF (10, 20) *)
  printf "%d\n" v;
  let v = (myUnCurry add) (10, 20) in
  printf "%d\n" v; 
