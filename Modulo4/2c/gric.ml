(* Esercizio 2c *)

let pairwith y xs =
	let rec aux lst = function
		| [] -> List.rev lst
		| x::rest -> aux ((y,x)::lst) rest
	in aux [] xs