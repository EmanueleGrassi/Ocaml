(* Esercizio 2e *)
(* num_di_stringhe: unit->int*)
(* questa funzione legge da input una sequenza di stringhe e calcola*)
(* quante ne sono state immesse*)
(* la funzione si ferma quando viene immessa la stringa vuota ("") *)

let num_di_stringhe () = 
	let rec aux n =
		let s = read_line()
		in match s with
		| "" -> n
		| _ -> aux (n+1)
in aux 0