(* Esercizio 3a*)
(* sumbetween: int->int->int*)
(* questa funziona calcola la somma di tutti i numeri compresi tra*)
(* n ed m (estremi inclusi) *)

let rec sumbetween n m = 
	if m = n 
	then m 
	else n + (sumbetween (n + 1) m)