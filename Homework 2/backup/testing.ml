type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;

let rec do_matching prod_func alt_list prefix derivation =
let rec find_match prod_func aux_prefix sym_list = match sym_list with
| [] -> 
	if List.length aux_prefix > 0 then 
		false
	else
		true
| sym_h::sym_t ->
        (match sym_h with
        | N element ->
                if do_matching prod_func (prod_func element) aux_prefix = true then
			(match aux_prefix with 
			| [] -> false
			| pre1_h::pre1_t ->
                        	find_match prod_func pre1_t sym_t)
                else
                        false
        | T element ->
		(match aux_prefix with 
		| [] -> false
		| pre2_h::pre2_t ->
			if Pervasives.compare pre2_h element = 0 then
                        	find_match prod_func pre2_t sym_t
                	else
                        	false))
in
match alt_list with
| [] -> false
| alt_h::alt_t ->
        if find_match prod_func prefix alt_h = true then
                true
        else
                do_matching prod_func alt_t prefix;;



let rec process_fragment gram accept frag prefix = match frag with
| [] -> None
| h::t ->
        if do_matching (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) (List.append prefix [h]) [] = true then
                (match accept t with
                | None -> process_fragment gram accept t (List.append prefix [h])
                | Some element -> Some element)
        else
                process_fragment gram accept t (List.append prefix [h]);;

let rec the_matcher gram accept frag =
process_fragment gram accept frag [];;

let make_matcher gram =
(fun gram -> the_matcher gram accept frag);;
