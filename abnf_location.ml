(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

open Lexing
open Printf

(* Keep track of our location *)
type t = {
    file_name: string option;
    line_num: int;
    column_num: int;
}

let start = ref 1
let line = ref 1
let file_name = ref None

let initial_location = { file_name=None; line_num=0; column_num=0 }
let current_location = ref initial_location

let start_parse f =
    start := 1;
    line := 1;
    file_name := Some f

let new_line lexbuf =
    start := lexeme_end lexbuf;
    incr line

let next_token lexbuf =
    let col = lexeme_start lexbuf - !start in
    let l = {file_name= !file_name; line_num= !line; column_num=col} in
    current_location := l;
    l
        
let string_of_location l =
    match l.file_name with
    |None -> ":"
    |Some f ->
        let c = if l.column_num > 0 then sprintf " char %d" l.column_num else
            "" in
        sprintf " at line %d%s:" l.line_num c

exception Parse_failure of (t * string)
