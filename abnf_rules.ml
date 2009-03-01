(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

open Abnf_syntaxtree
open Printf

exception Rule_error of string

let generate_rules rule_defs =
    let s_rule_hash = Hashtbl.create 1 in
    List.iter (fun rule_def ->
        (* Put all the rules into the syntax hash table *)
        if Hashtbl.mem s_rule_hash rule_def.s_name then 
            raise (Rule_error ("Duplicate rule: " ^ rule_def.s_name));
        Hashtbl.add s_rule_hash rule_def.s_name rule_def.s_rule;
    ) rule_defs;
    s_rule_hash

(* Go through the rules hash resolving references and making sure they are valid *)
let rec check_rule rules_hash rule_name =
    let cfn x = check_rule rules_hash rule_name x in
    function
    |S_terminal _ -> ()
    |S_string _ -> ()
    |S_concat (r1, r2) -> cfn r1; cfn r2
    |S_alt (r1, r2) -> cfn r1; cfn r2
    |S_seq (r1, r2) -> cfn r1; cfn r2
    |S_any_except (r1, r2) -> cfn r1; cfn r2
    |S_reference r ->
        if not (Hashtbl.mem rules_hash r) then
            raise (Rule_error (sprintf "Unknown reference: %s in rule %s" r rule_name))
    |S_repetition (min, max, r) -> begin
        cfn r;
        match min, max with
        |Some min, Some max ->
            if min > max then raise (Rule_error ("min > max in " ^ rule_name));
            if max < 0 then raise (Rule_error ("max < 0 in " ^ rule_name));
        |Some min, None ->
            if min < 0 then raise (Rule_error ("min < 0 in " ^ rule_name));
        |None, Some max ->
            if max < 0 then raise (Rule_error ("max < 0 in " ^ rule_name));
        |None, None -> ()
        end
    |S_hex_range (f,t) ->
        (* XXX put checks in *) ()

let check rules_hash = 
    Hashtbl.iter (fun rule_name rule ->
        check_rule rules_hash rule_name rule
    ) rules_hash
