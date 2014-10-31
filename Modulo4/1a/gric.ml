(* Esercizio 1a *)

let rec length = function
	| [] -> 0
	| x :: rest -> 1 + length rest;;

let length_coda l = 
	let rec aux n = function
		| [] -> n
		| x::rest -> aux (n+1) rest
	in aux 0 l