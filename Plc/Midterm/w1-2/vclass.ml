open Printf

class virtual shape (name: string) = object(s)
  val virtual mutable x: int
  val virtual mutable y: int

  method virtual area: int

  method describe =
    "the shape is " ^ name ^ " with the area of " ^ (string_of_int s#area) ^
    " units at coordiantes (" ^ string_of_int x ^ "," ^ string_of_int y ^ ")" 
end

class rectangle (w: int) (h: int) = object 

  inherit shape "rectangle" 
  val mutable x = 0
  val mutable y = 0

  method get_x = x
  method set_x v = x <- v

  method get_y = y
  method set_y v = y <- v

  method area = w * h
end 

class square (w: int) = object 

  inherit shape "square" 
  val mutable x = 0
  val mutable y = 0

  method get_x = x
  method set_x v = x <- v

  method get_y = y
  method set_y v = y <- v

  method area = w * w
end 

let main: unit = 
  let r = new rectangle 5 10 in
  r#set_x 0;
  r#set_y 15;
  let d = r#describe in
  printf "%s\n" d;
  let s = new square 7 in
  s#set_x 6;
  s#set_y 25;
  let d = s#describe in
  printf "%s\n" d;
