(* type definitions *)

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

(* convert_grammar gram1 *)

	(* <filtering> gets list of rules with matching nonterminal symbol *)
let filtering gram1 nt_symbol = 
List.filter (fun rule -> Pervasives.compare (Pervasives.fst rule) nt_symbol = 0) (Pervasives.snd gram1);;

	(* <production_function> separates rule list returned by <filtering> and separates
	 * the lhs and rhs into two lists, returning the list of rhs (alternative list) *)
let production_function gram1 nt_symbol = 
Pervasives.snd (List.split (filtering gram1 nt_symbol));;

	(* <convert_grammar> converts a hw1 style grammar into a hw2 style grammar *)
let convert_grammar gram1 = match gram1 with 
| (start_symbol, rules) -> (start_symbol, (fun nt_symbol -> production_function gram1 nt_symbol));;

(* parse_tree_leaves tree *)

	(* <build_leaf_pile> performs something like a depth first search to get leaves *)
let rec build_leaf_pile tree leaf_pile = match tree with
| [] -> leaf_pile
| h::t -> 
	(match h with 
	| Node (_, branches) -> build_leaf_pile t (build_leaf_pile branches leaf_pile)
	| Leaf fallen_leaf -> build_leaf_pile t (List.cons fallen_leaf leaf_pile));;

	(* <parse_tree_leaves gets a list of leaves with ordering from left to right *)
let parse_tree_leaves tree = 
List.rev (build_leaf_pile (List.cons tree []) []);;

(* make_matcher gram *)


(*
 * START OF WORK ZONE
 *)


let rec find_terminals nt_symbol fixed_rules rules = 
let rec process_rhs f_r rules sym_pair = match sym_pair with 
| (terminal, N element) -> 
| (terminal, T element) -> 
	if Pervasives.compare terminal element = 0 then 
		
	else
		
	

	(* <found_match> is an auxiliary function for <matching> *)
let rec found_match aux_list = match aux_list with 
| [] -> true
| h::t -> 
	if find_terminals h = true then
		found_match t
	else
		false;;

	(* <matching> looks for matching prefixes *)
let rec matching gram prefix = match gram with 
| [] -> false
| gram_h::gram_t ->
	if compare_lengths prefix gram_h = 0 then
		if found_match (List.combine prefix gram_h) = true then 
			true
		else
			matching gram_t prefix
	else
		matching gram_t prefix;;


(*
 * END OF WORK ZONE
 *)


	(* <process_fragment> process the fragment into prefixes and suffixes *)
let rec process_fragment gram accept frag prefix = match frag with 
| [] -> None
| h::t -> 
	if matching (Pervasives.snd (List.split gram)) (List.append prefix [h]) = true then
		(match accept t with 
		| None -> process_fragment gram accept t prefix
		| Some element -> Some element
	else
		process_fragment gram accept t prefix;;

	(* <the_matcher> finds matching prefixes and validates the match *)
let rec the_matcher gram accept frag = 
process_fragment gram accept frag [];;

	(* <make_matcher> returns a matcher for a given grammar *)
let make_matcher gram = 
(fun gram -> the_matcher gram accept frag);;

(* make_parser gram *)

	(* <the_parser> parses frag to get the optional parse tree *)
let rec the_parser gram frag = 
(*
 * Process grammar like in <found_match>
 *)

	(* <make_parser> returns a parser for a given grammar *)
let make_parser gram = 
(fun gram -> the_parser gram frag);;
