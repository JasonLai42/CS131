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
| (start_symbol, rules) -> (start_symbol, function nt_symbol -> 
					  production_function gram1 nt_symbol);;

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

	(* CHECK COMPLEXITY OF THIS WITH DENNIS *)

(* make_matcher gram *)



(* make_parser gram *)


