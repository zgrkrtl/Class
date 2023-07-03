{
    open Printf
    open Lexing

    exception BadChar of char
    
    type token =
      | BLANK : token
      | LPAREN: token
      | RPAREN: token
      | EOL   : token
      | IDENT : string -> token
      | NUM   : int    -> token
      | PLUS  : token
      | MINUS : token
      | MULT  : token
      | DIV   : token  
    
    let token2String(t: token): string =
      match t with
      | BLANK   -> "BLANK"
      | LPAREN  -> "LPAREN"
      | RPAREN  -> "RPAREN"
      | EOL     -> "EOL"
      | IDENT s -> "IDENT=[" ^ s ^ "]"
      | NUM i   -> "NUM=[" ^ (string_of_int i) ^ "]"
      | PLUS    -> "PLUS"
      | MINUS   -> "MINUS"
      | MULT    -> "MULT"
      | DIV     -> "DIV"
    
    let printToken(t: token): unit =
      printf "%s\n" (token2String t)

}

rule tokenize = parse
  | ' '
    { tokenize lexbuf }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | eof 
    { EOL }
  | [ 'a'-'z' 'A'-'Z' '_' ]+ as s 
    { IDENT s }
  | [ '0'-'9' ]+ as i
    { NUM (int_of_string i) } 
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { MULT }
  | '/'
    { DIV }
  | _ as c 
    { raise(BadChar c) }


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
      | x::xs -> token2String x ^ " " ^ tokenList2String xs
    
    let printTokenList(l: token list): unit =
      printf "%s\n" (tokenList2String l)

}