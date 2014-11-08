(* Esercizio 1f *)
(* reverse è una funzione che data una lista restituisce la stessa lista rovesciata*)
(* reverse: 'a list -> 'a list*)
(* reverse utilizza aux: 'a list -> 'a list -> a' list*)
(* che utilizza la prima lista parametro per costruire la lista risultato *)

let reverse_coda l = 
	let rec aux l = function
		| [] -> l
		| x::rest -> aux (x::l) rest
	in aux [] l 
	
	
	
let rec reverse = function
	| [] -> []
	| x::rest -> (reverse rest) @ x