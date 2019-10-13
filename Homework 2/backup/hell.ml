let accept_all string = Some string
let accept_empty_suffix = function 
	| _::_ -> None
	| x -> Some x

type english_language = 
| Sentence | NounPhrase | VerbPhrase | Article | Noun | Verb | Adjective | Adverb 
| Conjunction | Period

(* okay, a noun phrase alone is not a sentence, but oh well, it's to make it nontrivial *)
let english_rules =
		[Sentence, [N Sentence; N VerbPhrase; N Period]; 
		 Sentence, [N NounPhrase]; 
		 Sentence, [N Sentence; N Conjunction; N NounPhrase];
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
		 Conjunction, [T "but"]; 
		 Period, [T "."]]

let english_grammar_hw1 = (Sentence, english_rules)

let english_grammar_hw2 = (Sentence,
                        function
                        | Sentence -> 
				[[N Sentence; N VerbPhrase; N Period]; 
				 [N Sentence; N Conjunction; N NounPhrase]; [N NounPhrase]]
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
				[[T "and"]; [T "but"]]
			| Period ->
				[[T "."]])

(* Reordering of the rules *)
let english_grammar_hw2_v2 = (Sentence, 
		function 
		| Sentence -> 
			[[N NounPhrase]; [N Sentence; N VerbPhrase; N Period]; 
			 [N Sentence; N Conjunction; N NounPhrase]]
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
			[[T "and"]; [T "but"]]
		| Period ->
			[[T "."]])

let english_test_frag = ["the"; "lazy"; "cat"; "and"; "the"; "dog"; "are"; "happy"; "."]

let english_test_tree = (Node (Sentence, 
				[Node (Sentence, 
					[Node (Sentence, 
						[Node (NounPhrase, 
							[Node (Article, 
								[Leaf "the"]);
							Node (Adjective, 
								[Leaf "lazy"]);
							Node (Noun,
								[Leaf "cat"])])]);
					Node (Conjunction, 
						[Leaf "and"]);
					Node (NounPhrase, 
						[Node (Article, 
							[Leaf "the"]);
						Node (Noun, 
							[Leaf "dog"])])]);
				Node (VerbPhrase, 
					[Node (Verb,
						[Leaf "are"]);
					Node (Adjective, 
						[Leaf "happy"])]);
				Node (Period, 
					[Leaf "."])]))

(* Test: make_matcher gram *)

let make_matcher_test = 
	((make_matcher english_grammar_hw2_v2 accept_all english_test_frag)
	= Some ["and"; "the"; "dog"; "are"; "happy"; "."])

(* Test: make_parser gram *)

let parse_tree_leaves_test = 
	((parse_tree_leaves english_test_tree) = english_test_frag)

let make_parser_test = 
	((make_parser english_grammar_hw2 english_test_frag) = Some english_test_tree)
