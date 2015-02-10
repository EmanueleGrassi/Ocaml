(* Esercizio 4*)
(* num_di_foglie: 'a ntree -> int*)
(* è una funzione che dato un albero t calcola quante sono le foglie di t.*)
(* Per questo scopo si serve di sum*)
(* sum: int list -> int *)
(* è una funzione che data una lista di interi ritorna la somma di tutti gli elementi *)


let rec sum = function
	| [] -> 0
	| x::rest -> x + sum rest

let rec num_di_foglie = function
	| Tr(x,[]) -> 1
	| Tr(x,list) -> sum (List.map (function x -> num_di_foglie x) list)