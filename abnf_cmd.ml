(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

open Printf

type mode = 
    |Dot
    |Tsort
    |Text
    |HTML
    |Sexpr
    |Edges
    |Terminals
    |RDParse
    |Sig
    
let _ =
    let files = ref [] in
    let output_file = ref "output.html" in
    let mode = ref Text in
    let file_to_parse = ref "" in
    let starting_nonterminal = ref "" in 
    let parse = [
        "-sig", Arg.Unit (fun () -> mode := Sig), "Output the mli file";
        "-dot", Arg.Unit (fun () -> mode := Dot), "Output in DOT format";
        "-tsort", Arg.Unit (fun () -> mode := Tsort), "Output topological sort of parse nodes";
        "-edges", Arg.Unit (fun () -> mode := Edges), "Output edges of parse graph for tsort(1)";
        "-text", Arg.Unit (fun () -> mode := Text), "Output ABNF in text format";
        "-html", Arg.Unit (fun () -> mode := HTML), "Output ABNF in HTML format";
        "-sexpr", Arg.Unit (fun () -> mode := Sexpr), "Output ABNF in S-expression format";
        "-terminals", Arg.Unit (fun () -> mode := Terminals), "Output all terminals used";
	"-rdparse", Arg.Tuple([Arg.Unit (fun () -> mode := RDParse); Arg.Set_string file_to_parse; Arg.Set_string starting_nonterminal]), "Recursive-descent parse a given file with a given starting nonterminal";
        "-o", Arg.Set_string output_file, "Output file";
    ] in
    let usagestr = "Usage: abnf <options> input-file" in
    Arg.parse parse (fun x -> files := x :: !files) usagestr;

    if List.length !files < 1 then (Arg.usage parse usagestr; exit 1);
    
    List.iter (fun file ->
        eprintf "Processing file: %s\n" file;
        let fin = open_in file in
        let lexbuf = Lexing.from_channel fin in
        Abnf_location.start_parse file;
        let rules = try
            Abnf_parser.main Abnf_lexer.token lexbuf
        with 
            Abnf_location.Parse_failure (e,msg) -> begin
                eprintf "Syntax error%s near token '%s' \'%s\'\n"
                    (Abnf_location.string_of_location e) (Lexing.lexeme lexbuf) msg;
            exit 1;
            end
        in
        let all_rules = Abnf_rules.generate_rules rules in
        Abnf_rules.check all_rules;
        begin match !mode with
        |Sig -> Abnf_signature.dump all_rules;
        |Dot ->
            Abnf_ops.Graph.dump_nodes all_rules;
        |Tsort ->
            List.iter print_endline (Abnf_ops.Graph.topological_sort all_rules);
        |Terminals ->
            List.iter print_endline (Abnf_ops.Text.all_terminals all_rules);
        |Edges ->
            Abnf_ops.Graph.dump_edges all_rules;
        |Text ->
            List.iter (fun rule ->
                print_endline (Abnf_ops.Text.string_of_rule_definition rule)
            ) rules;
        |Sexpr ->
            List.iter (fun rule ->
                print_endline (Abnf_ops.Text.sexpr_of_rule_definition rule)
            ) rules;
        |HTML ->
            let tsorted_nodes = Abnf_ops.Graph.topological_sort all_rules in
            printf "<html><head><link href=\"rules.css\" rel=\"stylesheet\" type=\"text/css\"></head><body>\n";
            printf "<table><tr><th class=\"rule_name\" width=\"20%%\">Rule Name</th><th class=\"rule_def\">Definition</th></tr>\n";
            List.iter (fun rule_name ->
                let rule_def = Hashtbl.find all_rules rule_name in
                printf "<tr><td class=\"rule_name\">\n";
                printf "<a name=\"rule-%s\" />%s" rule_name rule_name;
                printf "</td><td class=\"rule_def\">%s</td></tr>\n" (Abnf_ops.HTML.html_of_rule rule_def);
            ) tsorted_nodes;
            printf "</body></html>"
	|RDParse ->
	   Abnf_ops.Text.prettyprint_derivation 0 (Abnf_recursive_descent.parse_file_with_grammar 
						     !file_to_parse 
						     all_rules !
						     starting_nonterminal
						  )
        end;
        eprintf "Done parsing file: %s\n" file;
        ()
    ) !files;
    exit 0
