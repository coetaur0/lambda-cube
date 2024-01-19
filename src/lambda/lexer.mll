{
open Parser

exception Error

let newline lexbuf =
  let open Lexing in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

let whitespace = [' ' '\t']+

let newline = '\n' | '\r' | "\r\n"

let name = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*

let int = ['0'-'9']+

rule token =
  parse
  | whitespace { token lexbuf }
  | newline    { newline lexbuf; token lexbuf }
  | "lambda"   { LAMBDA }
  | "let"      { LET }
  | "in"       { IN }
  | "if"       { IF }
  | "then"     { THEN }
  | "else"     { ELSE }
  | "succ"     { SUCC }
  | "pred"     { PRED }
  | "Nat"      { NAT }
  | "Bool"     { BOOL }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "->"       { ARROW }
  | '='        { EQ }
  | '~'        { NOT }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '{'        { LBRACE }
  | '}'        { RBRACE }
  | ':'        { COLON }
  | ','        { COMMA }
  | '.'        { DOT }
  | name       { NAME (Lexing.lexeme lexbuf) }
  | int        { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | _          { raise Error }
  | eof        { EOF }
