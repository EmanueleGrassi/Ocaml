(* Esercizio 10a *)
(* stessa_struttura: 'a tree -> 'a tree -> bool *)
(* questa funzione verifica che due alberi abbiano la stessa struttura *)
(* ovvero sono identici ad eccezione delle etichette.*)

let rec stessa_struttura t1 t2 = 
	match t1 with
	| Empty -> (match t2 with
	            | Empty -> true
							| _ -> false)
	| Tr(x,a,b) -> (match t2 with
	                | Empty -> false
									| Tr(xx,aa,bb) -> stessa_struttura a aa && stessa_struttura b bb)