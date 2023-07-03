{
    open Printf
    open Lexing

    exception BadChar of char

    type token =
      | EOL   : token
      | LPAREN: token
      | RPAREN: token
      | COMMA : token
      | USCORE: token
      | NUM   : int -> token 

    let rec token2String(t: token): string =
    match t with
      | EOL    -> "EOL"
      | LPAREN -> "LPAREN"
      | RPAREN -> "RPAREN"
      | COMMA  -> "COMMA"
      | USCORE -> "USCORE"
      | NUM i  -> "NUM=[" ^ string_of_int i ^ "]"

    let printToken(t: token): unit =
      printf"%s\n" (token2String t)
}

rule tokenize = parse
  | ' '
    { tokenize lexbuf }
  | "\n"
    { tokenize lexbuf }
  | "\t"
    { tokenize lexbuf }
  | "\c"
    { tokenize lexbuf }
  | eof
    { EOL }
 | '['
    { LPAREN }
 | ']'
    { RPAREN }
 | '_'
    { USCORE }
 | ','
    { COMMA }
 | [ '0'-'9' ]+ as i
    { NUM (int_of_string i) }
 | _ as c
    { raise (BadChar c) }

{
    let rec getTokensFromBuffer(b: lexbuf): token list =
      let tkn = tokenize b in
        match tkn with
        | EOL -> [EOL]
        | t   -> t :: getTokensFromBuffer b

    let getTokensFromString(s: string): token list =
      getTokensFromBuffer(from_string s)

    let rec tokenList2String(l: token list): string =
      match l with
      | []    -> ""
      | x::xs -> token2String x  ^  " " ^ tokenList2String xs

    let printTokenList(l: token list): unit =
      printf"%s\n" (tokenList2String l)
}