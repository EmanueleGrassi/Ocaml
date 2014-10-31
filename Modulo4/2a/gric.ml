(* Esercizio 2a *)

let copy n elem =
	let rec aux lst = function
		| 0 -> lst
		| n -> aux (elem::lst) (n-1)
	in aux [] n