(* Esercizio 11 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception E

let rec path p = function
	| Empty -> raise E
	| Tr(x,Empty,Empty) -> if p x then [] else [x]
	| Tr(x,a,b) -> if p x
	               then raise E
								 else (match a with
								      | Empty -> x::(path p b)
								      | Tr(y,Empty,Empty) -> if p y then x::(path p b) else x::(path p a)
											| Tr(y,aa,bb) -> try x::(path p a)
											                 with E -> x::(path p b))