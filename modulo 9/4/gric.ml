(* Esercizio 4 *)
(* complementare: form -> form*)
(* è una funzione che data una formula, riporta la stessa formula, a cui*)
(* i singoli atomi sono stati sostituiti dai rispettivi atomi complementari*)


let rec complementare = function
	| True -> False
	| False -> True
	| Prop s -> Not(Prop s)
	| Not f -> f
	| And (f,g) -> And(complementare f, complementare g)
	| Or (f,g) -> Or(complementare f, complementare g)
	| Imp (f,g) -> Imp(complementare f, complementare g)