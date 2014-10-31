(* Esercizio 1b *)

let rec sumof = function 
	| [] -> 0
	| x::rest -> x + sumof rest

let sumof_coda l = 
	let rec aux f = function
		| [] -> f
		| x::rest -> aux (x+f) (rest)
	in aux 0 l