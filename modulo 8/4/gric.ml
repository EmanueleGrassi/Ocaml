(* Esercizio 4 *)
(* num_foglie: 'a tree -> int *)
(* questa funzione ritorna il numero della foglie dell'albero *)

let rec numero_foglie = function
	| Empty -> 0
	| Tr(x,Empty,Empty) -> 1
	| Tr(_,a,b) -> numero_foglie a + numero_foglie b