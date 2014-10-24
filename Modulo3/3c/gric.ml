(* Esercizio 3c *)

let rec power e n =
	match n with
	0 | 1 -> e
	| _ -> e * power e (n-1)

