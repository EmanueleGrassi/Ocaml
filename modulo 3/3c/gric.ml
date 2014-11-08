(* Esercizio 3c *)
(* power: int->int->int *)
(* questa funzione calcola la potenza con base e ed esponente n *)

let rec power e n =
	match n with
	0 | 1 -> e
	| _ -> e * power e (n-1)

