(* Test: subset a b *)

let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [1;2;3]
let my_subset_test2 = subset [2;2;3;2] [2;3;3]
let my_subset_test3 = subset [1] [3;1;2;4]
let my_subset_test4 = not (subset [1;6] [1;9;8])
let my_subset_test5 = not (subset [7;7;7] [1;2;3;4;5;6;8;9])

(* Test: equal_sets a b *)

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [1] [1;1;1]
let my_equal_sets_test2 = equal_sets [2;2;3;3;4;5;5;5] [2;2;2;3;4;4;5]
let my_equal_sets_test3 = not (equal_sets [] [1;2;3])
let my_equal_sets_test4 = not (equal_sets [7;8;9] [4;5;8;9])

(* Test: set_union a b *)

let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [1] [2;3]) [1;2;3]
let my_set_union_test2 = equal_sets (set_union [1;1] [1;1;1]) [1]
let my_set_union_test3 = equal_sets (set_union [1;2;3] []) [1;2;3]
let my_set_union_test4 = equal_sets (set_union [1;2;4] [3;5;6]) [1;2;3;4;5;6]

(* Test: set_intersection a b *)

let my_set_intersection_test0 = equal_sets (set_intersection [] []) []
let my_set_intersection_test1 = equal_sets (set_intersection [1;2;3] [4;5;6]) []
let my_set_intersection_test2 = 
		equal_sets (set_intersection [1;3;6;6] [1;1;6]) [1;6]
let my_set_intersection_test3 = equal_sets (set_intersection [2;1] [4;4;2;1]) [1;2]
let my_set_intersection_test4 = 
		equal_sets (set_intersection [1;2;3;4] [4;2;3;1;5]) [1;2;3;4]

(* Test: set_diff a b *)

let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [1;2;3] [3;2;1]) []
let my_set_diff_test2 = equal_sets (set_diff [1;1;2;3;2] [3;1;3;2]) [] 
let my_set_diff_test3 = equal_sets (set_diff [1;3;8;6] [7;5;2;1]) [3;6;8]
let my_set_diff_test4 = equal_sets (set_diff [1;1;2;3] [2;2;1;4]) [3]

(* Test: computed_fixed_point eq f x *)

let my_computed_fixed_point_test0 = 
			computed_fixed_point (=) (fun x -> x * 2) 2 = 0
let my_computed_fixed_point_test1 = 
			computed_fixed_point (=) (fun x -> x + x) 1 = 0
let my_computed_fixed_point_test2 = 
			computed_fixed_point (=) sqrt 20. = 1.

(* Test: filter_reachable g *)

type teaching_assistants = | Tanmay | Briley | Zachary | Yuan

let ta_rules =
	[Tanmay, [T "H"; T "E"; T "L"; T "P"];
	Tanmay, [];
	Tanmay, [N Briley; N Tanmay; N Briley];
	Briley, [T "L"; T "I"; N Yuan; T "F"; T "E"];
	Briley, [];
	Briley, [N Briley; N Briley];
	Briley, [N Yuan; N Yuan; N Yuan];
	Zachary, [T "I"; T "S"; N Yuan];
	Zachary, [N Zachary; N Yuan];
	Yuan, [T "P"; N Yuan; T "A"; T "I"; T "N"];
	Yuan, [N Zachary];
	Yuan, []]

let ta_grammar = Tanmay, ta_rules

let my_filter_reachable_test0 = filter_reachable ta_grammar = ta_grammar
let my_filter_reachable_test1 = 
		filter_reachable (Briley, ta_rules) = 
			(Briley, 
			[Briley, [T "L"; T "I"; N Yuan; T "F"; T "E"];
		        Briley, [];
		        Briley, [N Briley; N Briley];
        		Briley, [N Yuan; N Yuan; N Yuan];
        		Zachary, [T "I"; T "S"; N Yuan];
        		Zachary, [N Zachary; N Yuan];
        		Yuan, [T "P"; N Yuan; T "A"; T "I"; T "N"];
        		Yuan, [N Zachary];
        		Yuan, []])
let my_filter_reachable_test2 = 
		filter_reachable (Zachary, ta_rules) = 
			(Zachary, 
			[Zachary, [T "I"; T "S"; N Yuan];
		        Zachary, [N Zachary; N Yuan];
		        Yuan, [T "P"; N Yuan; T "A"; T "I"; T "N"];
		        Yuan, [N Zachary];
		        Yuan, []])
