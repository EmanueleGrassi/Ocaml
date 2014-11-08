(* Esercizio 2c *)
(* tutti_minori: int -> bool*)
(* questa funzione legge da input una sequenza di stringhe numeriche*)
(* e verifica che tutti i numeri immessi siano minori del parametro n*)
(* La funzione si interrompe quando non viene immessa una stringa numerica.*)
(* se non viene immesso nessun numero, restituisce true *)

let tutti_minori n = 
	let rec aux v = 
		try let b = int_of_string(read_line())
				in aux (b<n && v)
	  with _ -> v
	in aux true