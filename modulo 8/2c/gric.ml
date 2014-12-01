(* Esercizio 2c *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec altezza = function
	| Empty -> 0
	| Tr(x,a,b) -> 1 + (max (altezza a) (altezza b))

let balanced = function
	| Empty -> true
	| Tr(x,a,b) -> abs(altezza a - altezza b) <= 1
