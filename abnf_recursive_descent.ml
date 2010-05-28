
open Char
open String
open List
open Abnf_ops
open Abnf_syntaxtree
open Printf

exception RecursiveDescentParseFailure of string

let rd_input_byte fin = 
  try
    input_byte fin
  with End_of_file ->
    raise (RecursiveDescentParseFailure "End of file reached")

let peek_byte fin = 
  let original_pos = pos_in fin in
  let ret = rd_input_byte fin in
    seek_in fin original_pos; ret

let parse_file_with_grammar infile grammar starting_nonterminal = (

  let fin = open_in infile in

  let rec make_int_range low high =
    if low > high then [] else low::(make_int_range (low+1) high)

  in let possible_chars_of_terminal = function
    | ALPHA -> (make_int_range (Char.code 'a') (Char.code 'z')) 
	@ (make_int_range (Char.code 'A') (Char.code 'Z'))
    | DIGIT -> (make_int_range (Char.code '0') (Char.code '9'))
    | HEXDIG -> (make_int_range (Char.code '0') (Char.code '9')) 
	@ (make_int_range (Char.code 'A') (Char.code 'F')) 
	@ (make_int_range (Char.code 'a') (Char.code 'f'))
    | DQUOTE -> [Char.code '\"']
    | SP -> [Char.code ' ']
    | LWSP -> [Char.code ' '; 9; 10; 13]
    | WSP -> [Char.code ' '; 9]
    | HTAB -> [9]
    | VCHAR -> make_int_range 0x21 0x7e
    | CHAR -> make_int_range 0x01 0x7f
    | OCTET -> make_int_range 0x00 0x7f
    | CTL -> 0x7f::(make_int_range 0x00 0x1f)
    | CR -> [0x0d]
    | LF -> [0x0a]
    | CRLF -> [0x0d; 0x0a]
    | BIT -> [Char.code '0'; Char.code '1']

  in let find_named_rule name =
      (Hashtbl.find grammar name)

  in let rec rule_nullable = (function
			     | S_terminal term -> (match term with
				 | LWSP -> true
				 | _ -> false)
			     | S_string str -> false
			     | S_concat (rl1, rl2) -> (rule_nullable rl1) && (rule_nullable rl2)
			     | S_alt (rl1, rl2) -> (rule_nullable rl1) || (rule_nullable rl2)
			     | S_seq (rl1, rl2) -> (rule_nullable rl1) && (rule_nullable rl2)
			     | S_repetition(min, max, rl) -> (match min with 
								| None | Some 0 -> true
								| Some n -> false)
			     | S_reference r -> rule_nullable (find_named_rule r)
			     | S_any_except (r1, r2) -> rule_nullable r1
			     | S_hex_range (f, t) -> false
			     )

  in let rec parse_terminal term = (match term with
				  | LWSP -> while (let next_byte = rd_input_byte fin in 
						     (next_byte = 9 || next_byte = 10 || next_byte = 13)) 
				    do () done
				  | CRLF -> (parse_terminal CR; parse_terminal LF)
				  | _ -> 
				      (let next_byte = rd_input_byte fin in
				       let possible_bytes = possible_chars_of_terminal term in
					 if exists (fun x -> x = next_byte) possible_bytes then 
					   (eprintf "Consumed %C\n" (chr next_byte);
					    ())
					 else raise (RecursiveDescentParseFailure
						       (sprintf "Terminal \"%s\" cannot accept %C" 
							  (Text.string_of_terminal term) 
							  (chr next_byte)
						       ))
				      )
				   )
				
  in let consume_string str = (for i = 0 to ((String.length str) - 1) do
				 let next_byte = rd_input_byte fin in
				   if next_byte = code str.[i] then () else
				     raise (RecursiveDescentParseFailure
				       (sprintf "%C does not match position %d of string \"%s\"" (chr next_byte) i str))
			       done
			      )

  in let rec parse_rule rule =
      (let restart_pos = pos_in fin in
	 try
	   (match rule with
	      | S_terminal term -> eprintf "Matching terminal %s\n" (Text.string_of_terminal term);
		  parse_terminal term
	      | S_string str -> eprintf "Consuming string %s\n" str; consume_string str
	      | S_concat (rl1, rl2) -> eprintf "Matching %s then %s\n" 
		  (Text.string_of_rule rl1) (Text.string_of_rule rl2); 
		  parse_rule rl1; parse_rule rl2
	      | S_alt (rl1, rl2) -> 
		  (eprintf "Choice: match %s or %s\nTrying %s\n"
		     (Text.string_of_rule rl1) (Text.string_of_rule rl2) (Text.string_of_rule rl1); 
		   (try parse_rule rl1 with RecursiveDescentParseFailure str ->
		      (eprintf "Matching %s failed: %s. Trying to match %s instead\n"
			 (Text.string_of_rule rl1) str (Text.string_of_rule rl2);
		       seek_in fin restart_pos;
		       (try 
			  parse_rule rl2
			with RecursiveDescentParseFailure str2 ->
			  raise (RecursiveDescentParseFailure (sprintf "Both \"%s\" and \"%s\"" str str2))
		       )
		      )
		   )
		  )
	      | S_seq (rl1, rl2) -> eprintf "Matching %s then %s\n"
		  (Text.string_of_rule rl1) (Text.string_of_rule rl2);
		  parse_rule rl1; parse_rule rl2
	      | S_repetition (min, max, rl) -> (
		  eprintf "Matching %s\n" (Text.string_of_rule (S_repetition (min, max, rl))); 
		  if max = Some 0 then () else
		    let may_skip = match min with | None -> true | Some 0 -> true | _ -> false in
		    let new_min = match min with | None -> None | Some 0 -> Some 0 | Some n -> Some (n - 1) in
		    let new_max = match max with | None -> None | Some 0 -> Some 0 | Some n -> Some (n - 1) in
		    let should_continue = ref true in
		      (eprintf "Trying matching against %s\n" (Text.string_of_rule rl);
		       (try parse_rule rl with RecursiveDescentParseFailure str ->
			  if may_skip then 
			    (eprintf "Matching %s failed, but skipping permitted\n" (Text.string_of_rule rl); 
			     seek_in fin restart_pos;
			     should_continue := false;
			     ())
			  else raise (RecursiveDescentParseFailure
					(sprintf "Failure parsing \"%s\"'s inner expression: %s"
					   (Text.string_of_rule rule)
					   str
					)
				     )
		       );
		       if !should_continue then
			 parse_rule (S_repetition (new_min, new_max, rl))
		       else
			 ()
		      )
		)
	      | S_reference r -> eprintf "Expanding %s to %s\n"
		  r (Text.string_of_rule (find_named_rule r));
		  parse_rule (find_named_rule r)
	      | S_any_except (r1, r2) -> 
		  eprintf "Matching against %s but not %s\n" 
		    (Text.string_of_rule r1) 
		    (Text.string_of_rule r2);
		  parse_rule r1;
		  let after_success_pos = pos_in fin in
		  let should_raise = ref true in
		    seek_in fin restart_pos;
		    eprintf "Successfully matched %s; checking can't match %s\n" 
		      (Text.string_of_rule r1) 
		      (Text.string_of_rule r2);
		    (try 
		       parse_rule r2
		     with RecursiveDescentParseFailure str -> 
		       eprintf "Good: failed to match %s with error \"%s\"\n" (Text.string_of_rule r2) str;
		       (* Put the file pointer where it was after the r1 matching success *)
		       seek_in fin after_success_pos;
		       should_raise := false
		    );
		    if !should_raise then
		      raise (RecursiveDescentParseFailure
			       (sprintf "Matched successfully against rule %s, but also %s, in an any-except context"
				  (Text.string_of_rule r1) (Text.string_of_rule r2)
			       )
			    )
		    else
		      ()
	      | S_hex_range (f, t) -> eprintf "Consuming byte between %x and %x\n" f t;
		  let next_byte = rd_input_byte fin in
		    if next_byte >= f && next_byte <= t then ()
		    else raise (RecursiveDescentParseFailure
				  (sprintf "Rule \"%s\" not satisfied by byte %C" 
				     (Text.string_of_rule (S_hex_range (f, t))) 
				     (chr next_byte)
				  )
			       )
	   )
	 with RecursiveDescentParseFailure str ->
	   seek_in fin restart_pos; (* Undo any side-effects on the file pointer *)
	   raise (RecursiveDescentParseFailure str) (* reraise! *)
      )
      
  in parse_rule (find_named_rule starting_nonterminal)
	    
)      
	
