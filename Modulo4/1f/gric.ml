(* Esercizio 1f *)

let reverse_coda l = 
	let rec aux l = function
		| [] -> l
		| x::rest -> aux (x::l) rest
	in aux [] l 