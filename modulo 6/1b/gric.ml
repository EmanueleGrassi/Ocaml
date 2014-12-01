(* Esercizio 1b *)
(* takewhile: ('a -> bool) -> 'a list -> 'a list *)
(* takewhile è una funzione che riporta la parte iniziale più lunga di una*)
(* data lista i cui elementi soddisfano il predicato p passato come parametro*)

let rec takewhile p = function
	| [] -> []
	| x::rest -> if p x
	             then x::(takewhile p rest)
							 else takewhile p []