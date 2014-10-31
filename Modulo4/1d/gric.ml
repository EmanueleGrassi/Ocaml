(* Esercizio 1d *)

let rec drop n lst = 
	try if n = 0 
			then lst
			else drop (n-1) (List.tl lst)
	with Failure "tl" -> []