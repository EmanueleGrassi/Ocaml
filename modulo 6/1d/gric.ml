(* Esercizio 1d *)
(* partition: ('a -> bool) -> 'a list -> ('a list * 'a list)*)
(* questa funzione crea una coppia di liste nella quali vengono inseriti*)
(* gli elementi che soddisfano p (nella prima) e quelli che non *)
(* lo soddisfano (nella seconda) *)

let rec partition p = function
	| [] -> ([],[])
	| x::rest -> if p x
               then (x::(fst (partition p rest)), snd (partition p rest))
							 else (fst (partition p rest), x::(snd (partition p rest)))