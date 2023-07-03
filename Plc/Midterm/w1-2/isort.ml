
open Printf
open String
open Stdlib

let insertionSort (a: int array): int array =
  let swap (i: int) (j: int): unit =
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
  in 
  let l = Array.length a - 1 in
  for i = 0 to l do
    let j = ref (i-1) in
    while (!j >= 0 && a.(!j) > a.(!j+1)) do
      swap (!j) (!j+1);
      j := !j - 1;
    done
  done;
  a

let insertionSortR (a: int array): int array =
  let swap (i: int) (j: int): unit =
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
  in 
  let l = Array.length a - 1 in
  for i = 0 to l do
    let rec insert (j: int): unit =
      if (j >= 0 && a.(j) > a.(j+1))
      then 
        begin 
          swap j (j+1); 
          insert (j-1) 
        end
      else ()
    in 
    insert (i-1)
  done;
  a

let array2String (a: int array): string =
  let l = Array.length a - 1 in
  let s = ref "" in
  for i = 0 to l do
    if i < l 
    then s := !s ^ string_of_int a.(i) ^ ";"
    else s := !s ^ string_of_int a.(i)
  done;
  !s

let printArray (a: int array): unit =
  printf "%s\n" ("[|" ^ array2String a ^ "|]")

let main: unit =
  let a = insertionSortR [| 5;3;2;5;0;6;9;4;3;1;(-10) |] in
  printArray a;

