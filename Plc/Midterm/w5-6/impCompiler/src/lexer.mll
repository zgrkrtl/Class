{
    open Printf
    open Lexing

    exception BadChar of char

(*
  int a;
  int res ;
  res := 1;
  a := 6;
  while (a > 1)
  {
    res := a ∗ res ;
    a:= a − 1;
    skip
  }
*)

    type token =
      | EOL     : token
      | LPAREN  : token
      | RPAREN  : token
      | LBRACE  : token
      | RBRACE  : token
      | IDENT   : string -> token
      | BOOL    : bool   -> token
      | NUM     : int    -> token
      | PLUS    : token
      | MINUS   : token
      | MULT    : token
      | DIV     : token
      | AND     : token
      | OR      : token
      | EQ      : token
      | GT      : token
      | LT      : token
      | NEG     : token
      | SKIP    : token
      | INT     : token
      | ASSIGN  : token
      | SEQUENCE: token
      | IF      : token
      | THEN    : token
      | ELSE    : token
      | WHILE   : token

    let token2String(t: token): string =
      match t with
      | EOL      -> "EOL"  
      | LPAREN   -> "LPAREN"
      | RPAREN   -> "RPAREN"
      | LBRACE   -> "LBRACE"
      | RBRACE   -> "RBRACE"
      | IDENT s  -> "IDENT=[" ^ s ^ "]"
      | BOOL b   -> "BOOL=[" ^ string_of_bool b ^ "]"
      | NUM i    -> "NUM=[" ^ string_of_int i ^ "]"
      | PLUS     -> "PLUS"
      | MINUS    -> "MINUS"
      | MULT     -> "MULT"
      | DIV      -> "DIV"
      | AND      -> "AND"
      | OR       -> "OR"
      | EQ       -> "EQ"
      | GT       -> "GT"
      | LT       -> "LT"
      | NEG      -> "NEG"
      | SKIP     -> "SKIP"
      | INT      -> "INT"
      | ASSIGN   -> "ASSIGN"
      | SEQUENCE -> "SEQUENCE"
      | IF       -> "IF"
      | THEN     -> "THEN"
      | ELSE     -> "ELSE"
      | WHILE    -> "WHILE"

   let printToken(t: token): unit =
     printf "%s\n" (token2String t)
}

rule tokenize = parse
  | ' '
    { tokenize lexbuf }
  | "\n"
    { tokenize lexbuf }
  | "\r"
    { tokenize lexbuf }
  | "\t"
    { tokenize lexbuf }
  | eof
    { EOL }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '{'
    { LBRACE }
  | '}'
    { RBRACE }
  | '+'
    { PLUS }
  | '-'
    { MINUS }
  | '*'
    { MULT }
  | '/'
    { DIV }
  | "&&"
    { AND }
  | "||"
    { OR }
  | '='
    { EQ }
  | '>'
    { GT }
  | '<'
    { LT }
  | '~'
    { NEG }
  | "skip"
    { SKIP }
  | "int"
    { INT }
  | ":="
    { ASSIGN }
  | ';'
    { SEQUENCE }
  | "if"
    { IF }
  | "then"
    { THEN }
  | "else"
    { ELSE }
  | "while"
    { WHILE }
  | [ '0'-'9' ]+ as i
    { NUM (int_of_string i) }
  | ( "true" | "false" ) as b
    { BOOL (bool_of_string b) }
  | [ 'a'-'z' 'A'-'Z' '_' ]+ as s
    { IDENT s }  
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
      | x::xs -> token2String x  ^  " " ^ tokenList2String xs

    let printTokenList(l: token list): unit =
      printf"%s\n" (tokenList2String l)
}