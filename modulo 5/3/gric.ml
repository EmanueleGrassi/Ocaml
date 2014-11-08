(* Esercizio 3 *)

let explode str =
	let rec aux tmp = function
		| (-1) -> tmp
		| n -> aux (str.[n]::tmp) (n-1)
	in aux [] ((String.length str) -1)
	
let implode lst =
	let rec aux tmp = function
		| [] -> tmp
		| x::rest -> aux (tmp ^ (String.make 1 x) ) rest
	in aux "" lst