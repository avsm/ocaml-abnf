(** Copyright (c) 2008,2009 Anil Madhavapeddy <anil@recoil.org>
 ** See the COPYING file included in this distribution for licensing details *)

open Printf

open Abnf_syntaxtree

module Text = struct   
    let string_of_terminal = function
        | ALPHA -> "ALPHA"
        | DIGIT -> "DIGIT"
        | HEXDIGIT -> "HEXDIGIT"
        | DQUOTE -> "DQUOTE"
        | SP -> "SP"
        | LWSP -> "LWSP"
        | WSP -> "WSP"
        | HTAB -> "HTAB"
        | VCHAR -> "VCHAR"
        | CHAR -> "CHAR"
        | OCTET -> "OCTET"
        | CTL -> "CTL"
        | CR -> "CR"
        | LF -> "LF"
        | CRLF -> "CRLF"
        | BIT -> "BIT"
        | UPALPHA -> "UPALPHA"
        | LOALPHA -> "LOALPHA"

    let terminal_of_string = function
        | "ALPHA" -> Some ALPHA
        | "DIGIT" -> Some DIGIT
        | "HEXDIGIT" -> Some HEXDIGIT
        | "DQUOTE" -> Some DQUOTE
        | "SP" -> Some SP
        | "HTAB" -> Some HTAB
        | "WSP" -> Some WSP
        | "LWSP" -> Some LWSP
        | "VCHAR" -> Some VCHAR
        | "CHAR" -> Some CHAR
        | "OCTET" -> Some OCTET
        | "CTL" -> Some CTL
        | "CR" -> Some CR
        | "LF" -> Some LF
        | "CRLF" -> Some CRLF
        | "BIT" -> Some BIT
        | "UPALPHA" -> Some UPALPHA
        | "LOALPHA" -> Some LOALPHA
        | _ -> None
    
    let rec string_of_rule = function
        | S_terminal term -> string_of_terminal term
        | S_string str -> sprintf "\"%s\"" str
        | S_concat (rl1,rl2) -> sprintf "%s %s" (string_of_rule rl1) (string_of_rule rl2)
        | S_alt (rl1,rl2) -> sprintf "%s / %s" (string_of_rule rl1) (string_of_rule rl2)
        | S_bracket rl -> sprintf "(%s)" (string_of_rule rl)
        | S_element_list (min, max, rl) -> string_of_repeat_rule "#" min max rl
        | S_repetition (min, max, rl) -> string_of_repeat_rule "*" min max rl
        | S_reference r -> sprintf "@%s" r
        | S_any_except (r1,r2) -> sprintf "< any %s except %s >" (string_of_rule r1) (string_of_rule r2)
        | S_hex_range (f,t) -> sprintf "%%x%d-%d" f t
    and string_of_repeat_rule op min max rl =
        match min, max with
        |Some 0, Some 1 | None, Some 1-> sprintf "[%s]" (string_of_rule rl)  (* Optional element *)
        |Some 0, None | None, None -> sprintf "%s%s" op (string_of_rule rl)
        |Some min, None -> sprintf "%d%s%s" min op (string_of_rule rl)
        |Some 0, Some max | None, Some max -> sprintf "%s%d%s" op max (string_of_rule rl)
        |Some min, Some max -> sprintf "%d%s%d%s" min op max (string_of_rule rl)

    let rec sexpr_of_rule = function
        | S_terminal term -> string_of_terminal term
        | S_string str -> sprintf "(string %s)" str
        | S_concat (rl1,rl2) -> sprintf "(concat %s %s)" (sexpr_of_rule rl1) (sexpr_of_rule rl2)
        | S_alt (rl1,rl2) -> sprintf "(alt %s %s)" (sexpr_of_rule rl1) (sexpr_of_rule rl2)
        | S_bracket rl -> sprintf "(bracket %s)" (sexpr_of_rule rl)
        | S_element_list (min, max, rl) -> sprintf "(list %d %s %s)" (match min with |None -> 0 |Some x -> x)
            (match max with |None -> "inf" |Some x -> string_of_int x) (sexpr_of_rule rl)
        | S_repetition (min, max, rl) -> sprintf "(rep %d %s %s)" (match min with |None -> 0 |Some x -> x)
            (match max with |None -> "inf" |Some x -> string_of_int x) (sexpr_of_rule rl)
        | S_reference r -> sprintf "(ref %s)" r
        | S_any_except (r1,r2) -> sprintf "(anyexcept %s %s)" (sexpr_of_rule r1) (sexpr_of_rule r2)
        | S_hex_range (f,t) -> sprintf "(hexrange %d %d)" f t
    
    let string_of_rule_definition rd =
        sprintf "%s = %s" rd.s_name (string_of_rule rd.s_rule)

    let sexpr_of_rule_definition rd =
        sprintf "(%s %s)" rd.s_name (sexpr_of_rule rd.s_rule)
        
    let all_terminals rds =
        let terms = Hashtbl.create 1 in
        Hashtbl.iter (fun _ rule ->
            let rec register_terminal = function
            | S_terminal term -> Hashtbl.replace terms term ()
            | S_concat (rl1, rl2) |S_alt (rl1,rl2)
            | S_any_except(rl1,rl2) -> register_terminal rl1; register_terminal rl2
            | S_string _ | S_hex_range _ | S_reference _ -> ()
            | S_bracket(rl)
            | S_element_list (_,_,rl) -> register_terminal rl
            | S_repetition (_,_,rl) -> register_terminal rl
            in register_terminal rule
        ) rds;
        Hashtbl.fold (fun k _ a -> (string_of_terminal k) :: a) terms []

    let rec prettyprint_derivation offset = function
      | D_terminal (term, str) -> 
	  printf "%s%s: %S\n" (String.make offset ' ') (string_of_terminal term) str
      | D_string (str) -> 
	  printf "%s\"%s\"\n" (String.make offset ' ') str
      | D_hex_range (low, high, str) -> 
	  printf "%s%x-%x: %S\n" (String.make offset ' ') low high str
      | D_concat (d1, d2) -> 
	  prettyprint_derivation offset d1; 
	  prettyprint_derivation offset d2
      | D_reference (nt_name, d) -> 
	  printf "%s%s->\n" (String.make offset ' ') nt_name;
	  prettyprint_derivation (offset + 2) d
      | D_repetition (ds) ->
	  List.iter (prettyprint_derivation offset) ds
            
end

module Graph = struct
    (* each node is a string and a list of incoming edges of other rules *)
    type edge = { f: node; t: node } 
    (* oe: outgoing edge, ie: incoming edge *)
    and node = { n: string; mutable oe: edge list; mutable ie: edge list } 
    
    (* given the global hash table generate a node list *)
    (* lashed together with multiple unnecessary passes *)
    let generate_nodes h = 
        (* Fill in nodes *)
        let nodes = Hashtbl.create 1 in
        let edges = Hashtbl.create 1 in
        Hashtbl.iter (fun name nrule ->
            Hashtbl.add nodes name { n=name; ie=[]; oe=[] };
        ) h;
        (* return a node from the node list *)
        let find_node n = Hashtbl.find nodes n in
        (* Generate edges, based on which rules address other rules *)
        Hashtbl.iter (fun rule_name rule_def ->
            (* iterate over a list of named rules and return edges *)
            let rec edges_from_rule = function
            | S_terminal _ | S_string _ | S_hex_range _ -> []
            | S_reference torule -> 
                let from_node = find_node rule_name in
                let to_node = find_node torule in
                [ { f=from_node; t=to_node } ]
            | S_concat (rl1,rl2) | S_alt (rl1,rl2)
            | S_any_except (rl1,rl2) ->
                edges_from_rule rl1 @ (edges_from_rule rl2)
            | S_bracket (rl)
            | S_repetition (_,_,rl) | S_element_list (_,_,rl) -> edges_from_rule rl 
            in
            List.iter (fun x -> Hashtbl.add edges x ()) (edges_from_rule rule_def);
        ) h;
        (* Populate the nodes with their edges *)
        Hashtbl.iter (fun edge () ->
            let tonode = find_node edge.t.n in
            tonode.ie <- edge :: tonode.ie;
            let fnode = find_node edge.f.n in
            fnode.oe <- edge :: fnode.oe;
        ) edges;
        nodes, edges
    
    (* List of top-level nodes with no incoming edges *)
    let top_nodes h =
        let nodes,_ = generate_nodes h in
        (* Find the top-level nodes with no incoming nodes *)
        let top_nodes = Hashtbl.fold (fun k v a -> 
            match v.ie with |[] -> k :: a |_ -> a) nodes [] in
        top_nodes
        
    (* debugging dump node list in DOT format *)    
    let dump_nodes h =
        let nodes, edges = generate_nodes h in
        let dot_name = Str.global_replace (Str.regexp_string "-") "_" in
        printf "digraph G {\n";
        Hashtbl.iter (fun _ node ->
            printf  "%s;\n" (dot_name node.n);
            List.iter (fun edge ->
                printf "%s -> %s;\n" (dot_name edge.f.n) (dot_name edge.t.n)
            ) node.oe;
        ) nodes;
        printf "}\n%!"

    (* Dump the edges so it can be piped into tsort(1) *)
    let dump_edges h =
        let nodes, edges = generate_nodes h in
        Hashtbl.iter (fun e () ->
            printf "%s %s\n" e.f.n e.t.n
        ) edges
        
    (* output list of nodes topologically sorted.  *)
    let topological_sort h =
        let nodes, _ = generate_nodes h in
        let starting_node_number = Hashtbl.length nodes in
        let sorted = ref [] in
        (* Firstly, remove arcs pointing to the same node to ease cycle detection *)
        Hashtbl.iter (fun _ n ->
            n.oe <- List.filter (fun e -> e.t.n <> e.f.n) n.oe;
            n.ie <- List.filter (fun e -> e.t.n <> e.f.n) n.ie
        ) nodes;
        (* Set of empty nodes with no incoming edges to start the sort *)
        let empty_nodes = ref (Hashtbl.fold (fun _ node a ->
            if node.ie = [] then 
                node :: a else a) nodes []) in
        (* removes all nodes and only leaves cycles *)
        let tsort_nodes () =
            while List.length !empty_nodes > 0 do
                let n = List.hd !empty_nodes in
                assert (List.length n.ie = 0);
                empty_nodes := List.tl !empty_nodes;
                assert (not (List.mem n.n !sorted));
                sorted := n.n :: !sorted;
                Hashtbl.remove nodes n.n;
                List.iter (fun edge ->
                    (* remove this edge from the graph *)
                    edge.t.ie <- List.filter (fun e -> e.f.n <> edge.f.n || e.t.n <> edge.t.n) edge.t.ie;
                    edge.f.oe <- List.filter (fun e -> e.f.n <> edge.f.n || e.t.n <> edge.t.n) edge.f.oe;
                    if List.length edge.t.ie = 0 then
                        if not (List.exists (fun n -> edge.t.n = n.n) !empty_nodes) then
                            empty_nodes := edge.t :: !empty_nodes;
                ) n.oe;
            done;
        in
        tsort_nodes ();
        while Hashtbl.length nodes > 0 do
            (* cycles remaining, so remove some *)
            let cycles = ref [] in
            (* From node, traverse rest of graph looking for a cycle *)
            let rec find_cycles visited node =
                (* already been to this node? register a cycle *)
                if List.mem node.n visited then begin
                    if not (List.mem visited !cycles) then
                        cycles := (List.rev (node.n :: visited)) :: !cycles;
                end 
                (* descend down the other outgoing edges *)
                else
                    List.iter (fun e ->
                        find_cycles (node.n :: visited) e.t) node.oe
            in
            (* detect cycles across all nodes and sort by sorted cycle first *)
            Hashtbl.iter (fun _ node -> find_cycles [] node) nodes;
            cycles := List.sort (fun a b -> List.length a - (List.length b)) !cycles;
            (* we shouldnt be here at all if there arent any cycles *)
            assert(List.length !cycles > 1);
            let shortest_cycle = List.hd !cycles in
            assert(List.length shortest_cycle > 2);
            (* Break the shortest arc and update empty_nodes if appropriate *)
            let fromnode = Hashtbl.find nodes (List.nth shortest_cycle 0) in
            let tonode = Hashtbl.find nodes (List.nth shortest_cycle 1) in
            fromnode.oe <- List.filter (fun e -> e.t.n <> tonode.n) fromnode.oe;
            tonode.ie <- List.filter (fun e -> e.f.n <> fromnode.n) tonode.ie;
            if List.length tonode.ie = 0 then
                if not (List.exists (fun n -> tonode.n = n.n) !empty_nodes) then
                    empty_nodes := tonode :: !empty_nodes;
            (* Retry the tsort with remaining nodes *)
            tsort_nodes ();            
        done;
        assert (List.length !sorted = starting_node_number);
        !sorted
end

module HTML = struct
    let html_of_terminal t = 
        sprintf "<code>%s</code>" (Text.string_of_terminal t)

    let span_rule name = sprintf "<a href=\"#rule-%s\" class=\"rule\">%s</a>" name name
 
    let rec html_of_rule = function
        | S_terminal term -> html_of_terminal term
        | S_string str -> sprintf "<span class=\"string\">&quot;%s&quot;</span>" str
        | S_concat (rl1,rl2) -> sprintf "%s <span class=\"operator\">.</span> %s" (html_of_rule rl1) (html_of_rule rl2)
        | S_alt (rl1,rl2) -> sprintf "%s <span class=\"operator\">/</span> %s" (html_of_rule rl1) (html_of_rule rl2)
        | S_bracket rl -> sprintf " <span class=\"operator\">(</span> %s <span class=\"operator\">)</span> " (html_of_rule rl)
        | S_repetition (min, max, rl) -> html_of_repeat_rule "&#35;" min max rl
        | S_element_list (min, max, rl) -> html_of_repeat_rule "*" min max rl
        | S_reference r -> span_rule r
        | S_any_except (r1,r2) -> sprintf "<span class=\"anyexcept\">&lt;Any %s except %s&gt;</span>" (html_of_rule r1) (html_of_rule r2)
        | S_hex_range (f,t) -> sprintf "<span class=\"hexrange\">%%x%d-%d</span>" f t

    and html_of_repeat_rule op min max rl =
        match min, max with
        |Some 0, Some 1 | None, Some 1-> sprintf "[ %s ]" (html_of_rule rl)  (* Optional element *)
        |Some 0, None | None, None -> sprintf "<span class=\"operator\">%s</span>%s" op (html_of_rule rl)
        |Some min, None -> sprintf "<span class=\"operator\">%d%s</span>%s" min op (html_of_rule rl)
        |Some 0, Some max | None, Some max -> sprintf "<span class=\"operator\">%s%d</span>%s" op max (html_of_rule rl)
        |Some min, Some max -> sprintf "<span class=\"operator\">%d%s%d</span>%s" min op max (html_of_rule rl)
    
end

