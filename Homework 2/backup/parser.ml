(* make_matcher gram *)

        (* <do_matching> returns a option type depending on if a fragment is accepted;
         * also iterates through alternative lists for a nonterminal symbol
         * <process_fragment> process the fragments with a right hand side; mutually
         * recurses with <do_matching> to expand rule expressions for matching and
         * matches are checked with acceptor *)
let rec do_matching start_symbol prod_func alt_list accept frag parse_tree =
let rec process_fragment prod_func sym_list accept frag branches = match sym_list with
| [] -> accept frag
| sym_h::sym_t ->
        (match frag with
        | [] -> None
        | frag_h::frag_t ->
                (match sym_h with
                | N sym -> do_matching sym prod_func (prod_func sym) (process_fragment prod_func sym_t accept) frag branches
                | T sym ->
                        if Pervasives.compare sym frag_h = 0 then
                                process_fragment prod_func sym_t accept frag_t (List.append branches [Leaf sym])
                        else
                                None))
in
match alt_list with
| [] -> None
| alt_h::alt_t ->
        (match process_fragment prod_func alt_h accept frag (List.append parse_tree [(start_symbol, alt_h)]) with
        | None -> do_matching start_symbol prod_func alt_t accept frag parse_tree
        | Some acceptor_return -> Some acceptor_return);;

let the_matcher gram accept frag =
do_matching (Pervasives.fst gram) (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) accept frag;;

let make_matcher gram = the_matcher gram;;

(* make_parser gram *)

let my_parser_acceptor derivation frag = match frag with 
| [] -> Some derivation
| _ -> None;;

        (* <the_parser> is an auxiliary function to make the currying clearer *)
let rec the_parser gram frag =
match (do_parsing (Pervasives.fst gram) (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram))) my_parser_acceptor frag [] with
| None -> None
| Some parse_tree -> Some parse_tree;;

        (* <make_parser> returns a parser for a given grammar *)
let make_parser gram = the_parser gram;;
