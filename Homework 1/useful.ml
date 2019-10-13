Function subset a b Prototype:

let find_match a b = match List.mem a b with
| true -> 0
| false -> 1;;

let rec check_state a b = match a with
| [] -> 0
| h::t -> find_match h b + check_state t b;;

let subset a b = match check_state a b with
| 0 -> true
| _ -> false;;

Function equal_sets a b Prototype:

let rec check_lists a b c = match c with
| [] -> 0
| h::t -> find_match h a + find_match h b + check_lists a b t;;

let equal_sets a b = match check_lists a b List.append a b with
| 0 -> true
| _ -> false;;

Other Function equal_sets a b Prototypes:

let compare_elem a b = match Pervasives.compare a b with
| 0 -> 0
| _ -> 1;;

let rec check_lists a b = 
match (List.sort Pervasives.compare a, List.sort Pervasives.compare b) with
| ([], []) -> 0;
| ([], _) -> 1;
| (_, []) -> 1;
| (h1::t1, h2::t2) -> compare_elem h1 h2 + check_lists t1 t2;;

let equal_sets a b = match check_lists a b with
| 0 -> true
| _ -> false;;

let rec check_lists a b = 
match (a, b) with
| ([], []) -> 0;
| ([], h2::t2) -> contains_elem h2 a + check_lists [] t2
| (h1::t1, []) -> contains_elem h1 b + check_lists t1 []
| (h1::t1, h2::t2) -> contains_elem h1 b + contains_elem h2 a + check_lists t1 t2;;

Function filter_reachable g Prototype:

let match_symbol sym (lhs, rhs) =
if lhs = sym then true
else false;;

let check_type a = match a with
| N (nonterminal) -> nonterminal
| T (terminal) -> terminal;;

let rec get_rule_list sym_list n_list = match sym_list with
| [] -> n_list
| h::t ->
(match check_type h with
| nonterminal ->
(match List.mem h

let rec get_sym_list sym rules sym_list = match rules with
| [] -> sym_list
| (lhs, rhs)::t ->
(match match_symbol sym (lhs, rhs) with
| true ->
| false -> get_sym_list sym t sym_list);;

let rec get_nonterminals rhs_list rules filtered_g = match rhs_list with
| [] ->
| h::t ->
(match h with
| N element ->
(match List.mem element filtered_g)
| T element -> filtering element rules

let check_type a = match a with
| N element -> true
| T element -> false;;

let rec process_rhs rhs rules reachable_set = match rhs with
| [] -> reachable_set
| h::t ->
        (match h with
        | N element ->
                (match List.mem element reachable_set with
                | true -> process_rhs t rules reachable_set
                | false -> process_rhs t rules (get_reachable_set element rules (List.cons element reachable_set)))
        | T element -> process_rhs t rules reachable_set);;

let rec get_reachable_set sym rules reachable_set = match rules with
| [] -> reachable_set
| (lhs, rhs)::t ->
        (match lhs with
        | sym -> get_reachable_set sym t (process_rhs rhs rules reachable_set)
        | _ -> get_reachable_set sym t reachable_set);;

let process_rhs sym reachables_set = match sym with
| N element ->
(match List.mem element reachable_set with
| true -> (true, element)
| false -> (false, element))
| T element -> (false, element);;
