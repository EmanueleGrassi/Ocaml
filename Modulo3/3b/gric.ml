(* Esercizio 3b *)

let rec sumto n =
	let rec sumbetween n m =
		if( m = n )
		then m
		else n + sumbetween (n+1) m
	in sumbetween 0 n