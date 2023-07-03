
open Printf

let x = 1 + 2
let y = x * x 

let a = ref 0
let b = ref 2

let i = 1

let main: unit =
 print_string "hello world!\n";
 print_int x;
 printf "\n";
 print_int y;
 printf "\n";
 print_int !a;
 printf "\n";
 print_int !b;
 printf "\n";
 b := !a+3;
 print_int !b;
 printf "\n";
 if i = 1 then print_int 100 else print_int 200;
 printf "\n";
 for ind = 1 to 5 do
  a := !a + ind
 done;
 print_int !a;
 printf "\n";
 let u = ref 0 in
 u := 
 begin 
  2 + 
  (if !a > 0 then 3 else 5) 
 end;
 print_int !u;
 printf "\n";
 let v = 
  let x = 1 in (let y = 5 in x+y) * (let z = 3 in x*z)
 in
 print_int v;
 printf "\n";