(* Esercizio 2e *)

let enumera xs =
	let rec aux y lst = function
		| [] -> List.rev lst
		| x::rest -> aux (y+1) ((y,x)::lst) rest
	in aux 0 [] xs