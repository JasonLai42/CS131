type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal;;

(* make_matcher gram *)

        (* <do_matching> returns a option type depending on if a fragment is accepted;
         * also iterates through alternative lists for a nonterminal symbol
         * <process_fragment> process the fragments with a right hand side; mutually
         * recurses with <do_matching> to expand rule expressions for matching and
         * matches are checked with acceptor *)
let rec do_matching start_symbol prod_func alt_list accept frag derivation =
let rec process_fragment prod_func sym_list accept frag branch = match sym_list with
| [] -> accept frag branch
| sym_h::sym_t ->
        (match frag with
        | [] -> None
        | frag_h::frag_t ->
                (match sym_h with
                | N sym -> do_matching sym prod_func (prod_func sym) (process_fragment prod_func sym_t accept) frag branch
                | T sym ->
                        if Pervasives.compare sym frag_h = 0 then
                                process_fragment prod_func sym_t accept frag_t branch
                        else
                                None))
in
match alt_list with
| [] -> None
| alt_h::alt_t ->
        (match process_fragment prod_func alt_h accept frag (List.append derivation [(start_symbol, alt_h)]) with
        | None -> do_matching start_symbol prod_func alt_t accept frag derivation
        | Some acceptor_return -> Some acceptor_return);;


let parse_this_path derivation = 
let rec draw_branch parent children rest_of_tree branch = match children with 
| [] -> (Node (parent, branch))
| child_h::child_t -> 
	(match child_h with 
	| N sym -> 
		(match rest_of_tree with 
		| [] -> (Node (parent, branch))
		| rot_h::rot_t -> 
			if Pervasives.compare sym (Pervasives.fst rot_h) = 0 then 
				draw_branch parent child_t rest_of_tree (List.append branch [(draw_branch (Pervasives.fst rot_h) (Pervasives.snd rot_h) rot_t [])])
			else 
				draw_branch parent children rot_t branch)
	| T sym -> 
		draw_branch parent child_t rest_of_tree (List.append branch [Leaf sym]))
in
match derivation with 
| None -> None
| Some derivation -> 
	(match derivation with 
	| [] -> None
	| h::t -> 
		Some (draw_branch (Pervasives.fst h) (Pervasives.snd h) t []));;


	(* <the_parser> is an auxiliary function to make the currying clearer
         * <parser_accept> modifies the acceptor passed to <do_matching> to only get the
         * derivation of a fragment *)
let rec the_parser gram frag =
let parser_acceptor frag derivation = match frag with
| [] -> Some derivation
| _ -> None
in
parse_this_path 
(do_matching (Pervasives.fst gram) (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) parser_acceptor frag []);;

        (* <make_parser> returns a parser for a given grammar *)
let make_parser gram = the_parser gram;;


(*
Some
 (Node (Expr,
        [Node (Term,
                [Node (Lvalue,
                        [Leaf "$";
                        Node (Expr,
                                [Node (Term,
                                        [Node (Num,
                                                [Leaf "1"])])])])
        ])]))
*)
(*
Some
 [(Expr, [N Term; N Binop; N Expr]);
        (Term, [N Lvalue; N Incrop]);
                (Lvalue, [T "$"; N Expr]);
                        (Expr, [N Term]);
                                (Term, [N Num]);
                                        (Num, [T "1"]);
                (Incrop, [T "++"]);
        (Binop, [T "-"]);
        (Expr, [N Term]);
                (Term, [N Num]);
                        (Num, [T "2"])]
*)
