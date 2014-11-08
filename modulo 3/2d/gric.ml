(* Esercizio 2d *)
(* occorre: int -> bool *)
(* questa funzione legge da input una sequenza di stringhe numeriche *)
(* e verifica che sia stato immesso almeno un numero uguale al parametro n*)
(* La funzione si interrompe quando non viene immessa una stringa numerica.*)
(* se non viene immesso nessun numero, restituisce false *)

let occorre n = 
	let rec aux v = 
		try let b = int_of_string(read_line())
				in aux ((b=n) || v)
	  with _ -> v
	in aux false