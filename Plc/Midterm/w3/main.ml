open Printf
open Lexing
open Lexer

let main: unit =
  let t = tokenize (from_string "[[]]burak") in
  printToken t;
  let tl = getTokensFromString "][2023[]]burak" in
  printTokenList tl;