(* Esercizio 3 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec foglie_in_lista lst = function
  	| Empty -> false
  	| Tr(x,Empty,Empty) -> List.mem x lst
		| Tr(_,Empty,b) -> foglie_in_lista lst b
		| Tr(_,a,Empty) -> foglie_in_lista lst a
  	| Tr(_,a,b) -> foglie_in_lista lst a && foglie_in_lista lst b