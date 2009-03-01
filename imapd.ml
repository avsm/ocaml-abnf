open Imap_rules
open Imap_terms
open Printf

let _ =
    let _ = [
        "a001 login mrc secret";
        "a002 select inbox";
        "a003 fetch 12 full";
    ] in
    let digits = ["1234a"] in
    List.iter (fun d ->
        eprintf "parsing: %s\n" d;
        let p = Imap_parser.t d in
        match Rules.Number.one p with
        |Some (p,num) ->
            eprintf "match: ";
            eprintf "%ld" num;
            eprintf "\n";
        |None -> eprintf "no match\n"
    ) digits;
    ()