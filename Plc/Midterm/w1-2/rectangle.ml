open Printf

class rectangle (w: int) (h: int) = object 

  val mutable x = 0
  val mutable y = 0

  method get_x = x
  method set_x v = x <- v

  method get_y = y
  method set_y v = y <- v

  method area = w * h
end

class square (w: int) = object 
  inherit rectangle w w as r

  method isContained (px,py) =
    x <= px && px <= w + x &&
    y <= py && py <= w + y
end

let main: unit =
  let s = new square 6 in
  let a = s#area in
  s#set_x 0;
  s#set_y 10;
  let b = s#isContained (2,16) in
  printf "area of s = %d\n" a;
  printf "%b\n" b;