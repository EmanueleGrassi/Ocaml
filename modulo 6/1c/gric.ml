(* Esercizio 1c*)
(* dropwhile: ('a -> bool) -> 'a list -> 'a list *)
(* dropwhile � una funzione che rimuove la parte iniziale pi� lunga di una*)
(* data lista i cui elementi soddisfano il predicato p passato come parametro*)

let rec dropwhile p = function
	| [] -> []
	| x::rest -> if p x
	             then dropwhile p rest 
							 else x::rest