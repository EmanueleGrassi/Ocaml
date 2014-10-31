(* Esercizio 2d *)

let duplica xs =
	let rec aux lst = function
		| [] -> List.rev lst
		| x::rest -> aux (x::x::lst) rest
	in aux [] xs