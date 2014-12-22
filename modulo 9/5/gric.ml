(* Esercizio 5*)
(* test_nnf: form -> bool*)
(* è una funzione che data una formula restituisce true se questa è in forma*)
(* normale negativa, false altrimenti.*)

let rec test_nnf = function 
	| True | False -> true
	| Prop s -> true
	| Not f -> (match f with
	            | Prop s -> true
							| _ -> false )
	| And (f,g) -> test_nnf f && test_nnf g
	| Or(f,g) -> test_nnf f && test_nnf g
	| Imp(_,_) -> false