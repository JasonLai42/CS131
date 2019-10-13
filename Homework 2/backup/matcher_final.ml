let accept_all string = Some string
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal;;


let rec do_matching prod_func alt_list accept frag =
let rec process_fragment prod_func sym_list accept frag = match sym_list with 
| [] -> accept frag
| sym_h::sym_t -> 
	(match frag with 
	| [] -> None
	| frag_h::frag_t -> 
		(match sym_h with 
		| N sym -> do_matching prod_func (prod_func sym) (process_fragment prod_func sym_t accept) frag
		| T sym -> 
			if Pervasives.compare sym frag_h = 0 then 
				process_fragment prod_func sym_t accept frag_t
			else
				None))
in
match alt_list with 
| [] -> None
| alt_h::alt_t ->
	(match process_fragment prod_func alt_h accept frag with 
	| None -> do_matching prod_func alt_t accept frag
	| Some acceptor_return -> Some acceptor_return);;

let the_matcher gram accept frag =
do_matching (Pervasives.snd gram) (Pervasives.snd gram (Pervasives.fst gram)) accept frag;;

let make_matcher gram = the_matcher gram;;


type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num;;

let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;
