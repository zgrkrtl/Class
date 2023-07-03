{
    open Printf
    open Lexing

    exception BadChar of char

    type token = 
      | BLANK : token
      | LPARAN: token
      | RPARAN: token
      | EOL   : token
      | IDENT : string -> token
      | NUM   : int    -> token 

    let token2String (t: token): string =
      match t with
      | BLANK   -> "BLANK"
      | LPARAN  -> "LPARAN"
      | RPARAN  -> "RPARAN"
      | EOL     -> "EOL"
      | IDENT s -> "IDENT[=" ^ s ^ "]"
      | NUM i   -> "NUM[=" ^ (string_of_int i) ^ "]"
    
    let printToken (t: token): unit =
      printf "%s\n" (token2String t)
}

rule tokenize = parse
  | ' '                              { BLANK }
  | '['                              { LPARAN }
  | ']'                              { RPARAN }
  | eof                              { EOL }
  | ['a'-'z' 'A'- 'Z' '_']+ as s     { IDENT s }
  | ['0'-'9']+ as i                  { NUM (int_of_string i) }
  | _ as c                           { raise (BadChar c) }

{
    let rec getTokensFromBuffer (b: lexbuf): token list =
    let tkn = tokenize b in
      match tkn with
      | EOL -> [EOL]
      | t   -> t :: getTokensFromBuffer b
    
    let getTokensFromString (s: string): token list =
      getTokensFromBuffer (from_string s)
    
    let rec tokenList2String (l: token list): string =
      match l with
      | []    -> ""
      | x::xs -> token2String x ^ " " ^ tokenList2String xs
    
    let printTokenList (l: token list): unit =
      printf "%s\n" (tokenList2String l)

}
