(* Esercizio 4 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec numero_foglie = function
	| Empty -> 0
	| Tr(x,Empty,Empty) -> 1
	| Tr(_,a,b) -> numero_foglie a + numero_foglie b