(* Esercizio 3a *)

let rec sumbetween n m = 
	if m = n 
	then m 
	else n + (sumbetween (n + 1) m)