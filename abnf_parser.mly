/* Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details */

%{
    open Abnf_location
    open Abnf_syntaxtree
    open Printf

    let parse_error msg =
        raise (Abnf_location.Parse_failure (!Abnf_location.current_location, msg))
        
    let id (x,_) = x
    let loc (_,x) = x
    let dec (x,_) = try int_of_string x with _ -> parse_error ("int_of_string: " ^ x)
    let hexsta (x,_) = try Scanf.sscanf x "%%x%x" (fun n -> n) with _ -> parse_error ("hex_of_string: " ^ x)
    let hexend (x,_) = try Scanf.sscanf x "-%x" (fun n -> n) with _ -> parse_error ("hex_of_string: " ^ x)

%}

%token <Abnf_location.t> EOL EOF NEWLINE
%token <string * Abnf_location.t> IDENTIFIER STRING
%token <string * Abnf_location.t> NUMBER
%token <string * Abnf_location.t> HEXRANGESTART HEXRANGEEND
%token <Abnf_location.t> LBRACE RBRACE LBRACKET RBRACKET SLBRACKET SRBRACKET
%token <Abnf_location.t> EQUALS SLASH STAR LESSTHAN GREATERTHAN ANY EXCEPT

%start main
%type <Abnf_syntaxtree.rule_definition list> main
%%
main:
  abnf_rules EOF { $1 }
;
abnf_rules:
 | abnf_rule NEWLINE abnf_rules { $1 :: $3 }
 | abnf_rule {[$1] }
 | { [] }
;

abnf_rule:
  | IDENTIFIER EQUALS rule_definition {{ Abnf_syntaxtree.s_name=id $1; s_rule=$3 }}
 ;

rule_definition:
  | rule_definition rule_definition { Abnf_syntaxtree.S_concat ($1, $2) }
  | rule_definition SLASH rule_definition { Abnf_syntaxtree.S_alt ($1, $3) }
  | STRING { Abnf_syntaxtree.S_string (id $1) }
  | rule_repetition { $1 }
  | rule_range { $1 }
  | rule_identifier { $1 }
;

rule_identifier:
| IDENTIFIER { match Abnf_ops.Text.terminal_of_string (id $1) with 
    |None -> Abnf_syntaxtree.S_reference (id $1) 
    |Some term -> Abnf_syntaxtree.S_terminal term }
| LBRACKET rule_definition rule_definition RBRACKET { Abnf_syntaxtree.S_seq ($2, $3) }
| LBRACKET rule_definition RBRACKET { $2 } 
| LESSTHAN ANY rule_definition EXCEPT rule_definition GREATERTHAN 
  { Abnf_syntaxtree.S_any_except ($3, $5) }
;

rule_repetition:
  | NUMBER STAR NUMBER rule_identifier { Abnf_syntaxtree.S_repetition (Some (dec $1), Some (dec $3), $4) }
  | STAR NUMBER rule_identifier { Abnf_syntaxtree.S_repetition (None, Some (dec $2), $3) }
  | NUMBER STAR rule_identifier { Abnf_syntaxtree.S_repetition (Some (dec $1), None, $3) }
  | NUMBER rule_identifier { Abnf_syntaxtree.S_repetition (Some (dec $1), Some (dec $1), $2) }
  | STAR rule_identifier { Abnf_syntaxtree.S_repetition (Some 0, None, $2) }
  | SLBRACKET rule_definition SRBRACKET { Abnf_syntaxtree.S_repetition (Some 0, Some 1, $2) }
;

rule_range:
  | HEXRANGESTART optional_range { Abnf_syntaxtree.S_hex_range ((hexsta $1), (match $2 with |None -> (hexsta $1) |Some x -> x)) }
  
optional_range:
  | { None }
  | HEXRANGEEND { Some (hexend $1) }
  
