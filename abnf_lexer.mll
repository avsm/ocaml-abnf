(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

{
open Abnf_location
open Abnf_parser
exception Eof
}

let decimal_literal = ['0'-'9'] ['0'-'9']*
let hex_literal_single = ['0'-'9'] | ['A'-'F'] | ['a'-'f']
let hex_literal = hex_literal_single hex_literal_single*
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_' '-']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let string_chars = lowercase | uppercase
let identifier_chars = string_chars (decimal_literal | string_chars)*


rule token = parse
| [ ' ' '\t' ] { token lexbuf }
| '\n'* { new_line lexbuf; NEWLINE(next_token lexbuf) }
| '{'   { LBRACE(next_token lexbuf) }
| '}'   { RBRACE(next_token lexbuf) }
| '('   { LBRACKET(next_token lexbuf) }
| ')'   { RBRACKET(next_token lexbuf) }
| '['   { SLBRACKET(next_token lexbuf) }
| ']'   { SRBRACKET(next_token lexbuf) }
| '<'   { LESSTHAN(next_token lexbuf) }
| '>'   { GREATERTHAN(next_token lexbuf) }
| '='   { EQUALS(next_token lexbuf) }
| '/'   { SLASH(next_token lexbuf) }
| '*'   { STAR(next_token lexbuf) }
| '%' 'x' hex_literal { HEXRANGESTART(Lexing.lexeme lexbuf, next_token lexbuf) }
| '-' hex_literal  { HEXRANGEEND(Lexing.lexeme lexbuf, next_token lexbuf) }
| "any" { ANY (next_token lexbuf) }
| "except" { EXCEPT(next_token lexbuf) }
| decimal_literal { NUMBER(Lexing.lexeme lexbuf, next_token lexbuf) }
| '\"'  { string_e (Buffer.create 256) lexbuf }
| identifier_chars { IDENTIFIER(Lexing.lexeme lexbuf, next_token lexbuf) }
| eof { EOF(next_token lexbuf) }
| _ { raise (Abnf_location.Parse_failure (!Abnf_location.current_location, "")) }
and string_e s = parse
| '\"' { STRING(Buffer.contents s, next_token lexbuf) }
| _ as x { Buffer.add_char s x; string_e s lexbuf }
