(* Esercizio 2a *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec reflect = function 
	| Empty -> Empty
	| Tr(x,a,b) -> Tr(x,reflect b, reflect a)