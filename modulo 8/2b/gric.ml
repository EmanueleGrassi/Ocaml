(* Esercizio 2b *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let fulltree k = 
	let rec aux x = function
		| 0 -> Empty
		| n ->  Tr(x, aux (2*x) (n-1), aux(2*x+1) (n-1))
	in aux 1 k