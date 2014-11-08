(* Esercizio 3b*)
(* questa funzione utilizza la funzione sumbetween definita nell'esercizio 3a*)
(* sumto: int->int*)
(* questa funzione somma tutti i numeri compresi tra 0 e il parametro n (incluso) *)

let rec sumto n =
	let rec sumbetween n m =
		if( m = n )
		then m
		else n + sumbetween (n+1) m
	in sumbetween 0 n