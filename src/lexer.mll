{
(********************************************************************)
(* ocaml-scribble - Lexer file                                      *)
(********************************************************************)
(* $Time-stamp: <Malo - 2012>$ *)

open Common
open Syntax
open Parser

module LE = Lexing

let debug = Common.debug "lexer"

let lexeme = LE.lexeme
let linestart = ref 0
let lineno = ref 1

let newline lexbuf : unit =
(*  debug "New line added" ; *)
  linestart := LE.lexeme_start lexbuf; 
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;} ;
  incr lineno

let info lexbuf : info =
(*  debug "Gathering lexeme information" ; *)
  let c1 = LE.lexeme_start lexbuf in
  let c2 = LE.lexeme_end lexbuf in
  (!lineno, c1 - !linestart),(!lineno, c2 - !linestart)

let error lexbuf s =
  debug ("Error generation: "^s) ;
  let (l1,c1),(l2,c2) = info lexbuf in
  let t = lexeme lexbuf in
  let s = Printf.sprintf " %d:%d-%d: %s" l1 c1 c2 s in
  if t = "" then (debug ("No lexeme found") ;raise (Syntax_error (s,((l1,c1),(l2,c2)))))
  else (debug ("Lexeme found: "^t) ;raise (Syntax_error (s^ ": " ^ t,((l1,c1),(l2,c2)))))

}

let blank = [' ' '\t']+
  let symbol = ['{' '}' '[' ']' '(' ')' ':' '\\' '/' '.' '#' '&' '?' '!'
                  '\'' '\"']
let identifier = [ 'a' - 'z' 'A' - 'Z' '_' ] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*
let extidentifier = 
  ( symbol | [ 'a' - 'z' 'A' - 'Z' '_'] ) (symbol | ['a' - 'z' 'A' - 'Z' '0' - '9' '_'])*
let digoperator = [ '0' - '9'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*

let anything = [ ^ '\n']

let newline = '\n' 

rule token = parse
  | blank         { token lexbuf }
  | "and"         { AND (info lexbuf) }
  | "as"          { AS (info lexbuf) }
  | "at"          { AT (info lexbuf) }
  | "by"          { BY (info lexbuf) }
  | "choice"      { CHOICE (info lexbuf) }
  | "continue"    { CONTINUE (info lexbuf) }
  | "from"        { FROM (info lexbuf) }
  | "global"      { GLOBAL (info lexbuf) }
  | "import"      { let extidentifier = importidentifier lexbuf in
                    IMPORT (info lexbuf,extidentifier) }
  | "interruptible" { INTERRUPTIBLE (info lexbuf) }
  | "local"       { LOCAL (info lexbuf) }
  | "or"          { OR (info lexbuf) }
  | "par"         { PAR (info lexbuf) }
  | "protocol"    { PROTOCOL (info lexbuf) }
  | "rec"         { REC (info lexbuf) }
  | "role"        { ROLE (info lexbuf) }
  | "sig"         { SIG (info lexbuf) }
  | "to"          { TO (info lexbuf) }
  | "with"        { WITH (info lexbuf) }
  | "//"          { commentline lexbuf; token lexbuf }
  | "="           { EQUAL (info lexbuf) }
  | ";"           { SEMI (info lexbuf) }
  | ":"           { COLON (info lexbuf) }
  | ","           { COMMA (info lexbuf) }
  | "("           { LPA (info lexbuf) }
  | ")"           { RPA (info lexbuf) }
  | "<"           { LAB (info lexbuf) }
  | ">"           { RAB (info lexbuf) }
  | "{"           { LCB (info lexbuf) }
  | "}"           { RCB (info lexbuf) }
  | eof		  { EOF (info lexbuf) }
  | identifier    { IDENTIFIER (info lexbuf,lexeme lexbuf) }
  | digoperator   { DIGOPERATOR (info lexbuf, lexeme lexbuf) }
  | newline       { newline lexbuf; token lexbuf }
  | "/*"          { comment lexbuf ; token lexbuf }
  | "//"          { commentline lexbuf ; token lexbuf }
  | _		  { error lexbuf "Unknown token" }

and comment = parse
  | "*/"          { () }
  | "//"          { commentline lexbuf }
  | newline       { newline lexbuf; comment lexbuf }
  | eof		  { error lexbuf "Unmatched '/*'" }
  | _             { comment lexbuf }

and commentline = parse
  | newline       { () }
  | _             { commentline lexbuf }

and importidentifier = parse
  | blank         { importidentifier lexbuf }
  | extidentifier { lexeme lexbuf }
  | _             { error lexbuf "Wrong identifier" }
