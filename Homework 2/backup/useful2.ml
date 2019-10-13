let rec build_leaf_pile tree leaf_pile = 
let rec climb_branch aux_branch leaf_pile = match 
in
match tree with
| Node (_, branches) -> climb_branch branches leaf_pile;;
| Leaf fallen_leaf -> List.cons fallen_leaf leaf_pile;;

(* ======================================================= *)

let rec do_matching gram alt_list prefix posble_match = matching sym_list with
| [] ->
| alt_h::alt_t ->
        (match alt_h with
        | [] ->
        | sym_h::sym_t ->
                (match sym_h with
                | N element -> do_matching gram (Pervasives.snd gram element) prefix posbl
e_match
                | T element -> 
                        (match prefix with 
                        | [] -> 
                        | pre_h::pre_t -> 
                                if Pervasives.compare pre_h element = 0 then  
                                        
(* ======================================================= *)

let rec do_matching prod_func alt_list prefix =
let rec find_match prod_func aux_prefix sym_list = match sym_list with
| [] -> true
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
        if do_matching (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) (List.append prefix [h]) = true then
                (match accept t with
                | None -> process_fragment gram accept t (List.append prefix [h])
                | Some element -> Some element)
        else
                process_fragment gram accept t (List.append prefix [h]);;

let rec the_matcher gram accept frag =
process_fragment gram accept frag [];;

let make_matcher gram =
(fun gram -> the_matcher gram accept frag);;

(* ======================================================= *)

let do_matching gram accept frag prefix =
let rec found_match prod_func alt_list accept frag = match alt_list with
| [] -> None
| alt_h::alt_t ->
        (match process_fragment prod_func alt_h accept frag with
        | None -> found_match prod_func alt_t accept frag
        | Some acceptor_return -> Some acceptor_return)
and let rec process_fragment prod_func sym_list accept frag = match sym_list with
| [] -> accept frag
| sym_h::sym_t ->
        (match frag with
        | [] -> None
        | frag_h::frag_t ->
                (match sym_h with
                | N sym -> found_match prod_func (prod_func sym) (process_fragment prod_func sym_t acceptor) frag
                | T sym ->
                        if Pervasives.compare sym frag_h = 0 then
                                process_fragment prod_func sym_t accept frag_t
                        else
                                None))
in
found_match (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) accept frag;;


let make_matcher gram =
(fun gram -> do_matcher gram accept frag);;

(* ======================================================= *)

let rec do_matching prod_func alt_list prefix =
let rec find_match prod_func aux_prefix sym_list = match sym_list with
| [] -> true
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

        (* <process_fragment> process the fragment into prefixes and suffixes *)
let rec process_fragment gram accept frag prefix = match frag with
| [] -> None
| h::t ->
        if do_matching (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) (List.append prefix [h]) = true then
                (match accept t with
                | None -> process_fragment gram accept t prefix
                | Some element -> Some element)
        else
                process_fragment gram accept t prefix;;

        (* <the_matcher> finds matching prefixes and validates the match *)
let rec the_matcher gram accept frag =
process_fragment gram accept frag [];;

        (* <make_matcher> returns a matcher for a given grammar *)
let make_matcher gram =
(fun gram -> the_matcher gram accept frag);;
