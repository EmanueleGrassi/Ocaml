(* Esercizio 2g *)

let alternate xs =
	let rec aux n lst = function
		| [] -> List.rev lst
		| x::rest -> if n mod 2 = 0
								 then aux (n+1) lst rest
								 else aux (n+1) (x::lst) rest
	in aux 0 [] xs