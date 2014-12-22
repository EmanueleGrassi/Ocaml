(* Esercizio 1 *)
(* complessita: form -> int*)
(* � una funzione che calcola la complessit� di una formula, ovvero il *)
(* numero degli operatori logici da cui � composta *)

let rec complessita = function
	| True | False -> 0
	| Prop s -> 0
	| Not f -> 1 + complessita f
	| And (f,g) -> 1 + complessita f + complessita g
	| Or (f,g) -> 1 + complessita f + complessita g
	| Imp (f,g) -> 1 + complessita f + complessita g