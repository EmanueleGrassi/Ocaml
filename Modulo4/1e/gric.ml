(* Esercizio 1e *)

let rec append l1 l2 = 
	if l1 = [] then l2
	else List.hd l1 :: append (List.tl l1) l2