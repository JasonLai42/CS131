let accept_all string = Some string
let accept_empty_suffix = function 
	| _::_ -> None
	| x -> Some x

type english_language = 
| Sentence | ManyNouns | NounPhrase | VerbPhrase | Article | Noun | Verb | Adjective 
| Adverb | Conjunction | Period

(* okay, a noun phrase alone is not a sentence, but oh well, it's to make it nontrivial *)
let english_rules =
                [Sentence, [N ManyNouns; N VerbPhrase; N Period];
		 Sentence, [N ManyNouns];
		 ManyNouns, [N NounPhrase; N Conjunction; N ManyNouns];
		 ManyNouns, [N NounPhrase];
                 NounPhrase, [N Noun];
                 NounPhrase, [N Adjective; N Noun];
                 NounPhrase, [N Article; N Noun];
                 NounPhrase, [N Article; N Adjective; N Noun];
                 VerbPhrase, [N Verb];
                 VerbPhrase, [N Verb; N Adjective];
                 VerbPhrase, [N Adverb; N Verb];
                 VerbPhrase, [N Verb; N Adverb];
                 Article, [T "the"];
                 Article, [T "a"];
                 Noun, [T "cat"];
                 Noun, [T "dog"];
                 Noun, [T "man"];
                 Noun, [T "car"];
                 Verb, [T "is"];
                 Verb, [T "sleeps"];
                 Verb, [T "runs"];
                 Verb, [T "corners"];
                 Verb, [T "are"];
                 Adjective, [T "lazy"];
                 Adjective, [T "happy"];
                 Adjective, [T "healthy"];
                 Adjective, [T "new"];
                 Adverb, [T "well"];
                 Adverb, [T "quickly"];
                 Conjunction, [T "and"];
                 Period, [T "."]]

let english_grammar_hw1 = (Sentence, english_rules)

(* Test: convert_grammar gram *)
let english_grammar_hw2 = (convert_grammar english_grammar_hw1)

let english_grammar_hw2_v1 = (Sentence,
                        function
                        | Sentence -> 
				[[N ManyNouns; N VerbPhrase; N Period]; [N ManyNouns]]
			| ManyNouns ->
				[[N NounPhrase; N Conjunction; N ManyNouns]; 
				 [N NounPhrase]]
			| NounPhrase -> 
				[[N Noun]; [N Adjective; N Noun]; [N Article; N Noun]; 
				 [N Article; N Adjective; N Noun]]
			| VerbPhrase -> 
				[[N Verb]; [N Verb; N Adjective]; [N Adverb; N Verb]; 
				 [N Verb; N Adverb]]
			| Article -> 
				[[T "the"]; [T "a"]]
			| Noun -> 
				[[T "cat"]; [T "dog"]; [T "man"]; [T "car"]]
			| Verb ->
				[[T "is"]; [T "sleeps"]; [T "runs"]; [T "corners"]; [T "are"]]
			| Adjective -> 
				[[T "lazy"]; [T "happy"]; [T "healthy"]; [T "new"]]
			| Adverb -> 
				[[T "well"]; [T "quickly"]]
			| Conjunction -> 
				[[T "and"]]
			| Period ->
				[[T "."]])

(* Reordering of the rules *)
let english_grammar_hw2_v2 = (Sentence, 
		function 
		| Sentence -> 
			[[N ManyNouns]; [N ManyNouns; N VerbPhrase; N Period]] 
		| ManyNouns -> 
			[[N NounPhrase]; [N NounPhrase; N Conjunction; N ManyNouns]]
		| NounPhrase -> 
			[[N Noun]; [N Adjective; N Noun]; [N Article; N Noun]; 
			 [N Article; N Adjective; N Noun]]
		| VerbPhrase ->
			[[N Verb; N Adverb]; [N Adverb; N Verb]; [N Verb; N Adjective]; 
			 [N Verb]]
		| Article -> 
			[[T "the"]; [T "a"]]
		| Noun -> 
			[[T "cat"]; [T "dog"]; [T "man"]; [T "car"]]
		| Verb -> 
			[[T "is"]; [T "sleeps"]; [T "runs"]; [T "corners"]; [T "are"]]
		| Adjective -> 
			[[T "lazy"]; [T "happy"]; [T "healthy"]; [T "new"]]
		| Adverb -> 
			[[T "well"]; [T "quickly"]]
		| Conjunction -> 
			[[T "and"]]
		| Period ->
			[[T "."]])

let english_test_frag = ["the"; "lazy"; "cat"; "and"; "the"; "dog"; "and"; "the"; 
			 "healthy"; "man"; "are"; "happy"; "."]

let english_test_tree = (Node (Sentence, 
				[Node (ManyNouns, 
					[Node (NounPhrase, 
						[Node (Article, 
							[Leaf "the"]);
						Node (Adjective, 
							[Leaf "lazy"]);
						Node (Noun,
							[Leaf "cat"])]);
					Node (Conjunction, 
						[Leaf "and"]);
					Node (ManyNouns,
						[Node (NounPhrase, 
					 		[Node (Article, 
								[Leaf "the"]);
							 Node (Noun, 
								[Leaf "dog"])]);
						Node (Conjunction, 
							[Leaf "and"]);
						Node (ManyNouns, 
							[Node (NounPhrase, 
								[Node (Article, 
									[Leaf "the"]); 
								Node (Adjective, 
									[Leaf "healthy"]); 
								Node (Noun, 
									[Leaf "man"])])])])]);
				Node (VerbPhrase, 
					[Node (Verb,
						[Leaf "are"]);
					Node (Adjective, 
						[Leaf "happy"])]);
				Node (Period, 
					[Leaf "."])]))

(* Test: convert_grammar gram *)

let convert_grammar_test = 
	((make_parser english_grammar_hw2 english_test_frag) = Some english_test_tree)

(* Test: parse_tree_leaves tree *)

let parse_tree_leaves_test =
        ((parse_tree_leaves english_test_tree) = english_test_frag)

(* Test: make_matcher gram *)

let make_matcher_test = 
	((make_matcher english_grammar_hw2_v2 accept_all english_test_frag)
	= Some ["and"; "the"; "dog"; "and"; "the"; "healthy"; "man"; "are"; "happy"; "."])

(* Test: make_parser gram *)

let make_parser_test = 
	((make_parser english_grammar_hw2_v1 english_test_frag) = Some english_test_tree)
