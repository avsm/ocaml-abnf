(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

(* basic syntax tree for ABNF *)
open Printf

type terminal =
    | ALPHA     (* [A-Z][a-z] *)
    | UPALPHA   (* [A-Z] *)
    | LOALPHA   (* [a-z] *)
    | DIGIT     (* [0-9] *)
    | HEXDIGIT  (* [0-9a-fA-F] *)
    | DQUOTE
    | SP
    | HTAB
    | WSP
    | LWSP
    | VCHAR
    | CHAR
    | OCTET
    | CTL       (* <any US-ASCII control character (0-31) and DEL (127)> *)
    | CR        (* <US-ASCII CR, carriage return (13)> *)
    | LF        (* <US-ASCII LF, linefeed (10)> *)
    | CRLF      (* CR LF *)
    | BIT 

(* Type of the rules syntax tree *)
type rule =
    | S_terminal of terminal (* Terminal character *)
    | S_string of string (* Flat string *)
    | S_concat of rule * rule (* Concatenation of rules *)
    | S_reference of string (* reference to another rule *)
    | S_alt of rule * rule (* Alt rules with a / *)
    | S_bracket of rule (* Brackets : Needed because of the printer *)
    | S_repetition of int option * int option * rule (* Repetition *)
    | S_element_list of int option * int option * rule (* List rule, RFC2068 2.1 *)
    | S_hex_range of int * int
    | S_any_except of rule * rule (* any rule except rule *)

type derivation = 
  | D_terminal of terminal * string
  | D_hex_range of int * int * string
  | D_string of string
  | D_concat of derivation * derivation
  | D_reference of string * derivation
  | D_repetition of derivation list
(* No any_except or alt, of which all branches are not represented, or seq/concat differentiation *)

(* Each line in an ABNF file is defined here *)
type rule_definition = {
    s_name: string;
    s_rule: rule;
}

