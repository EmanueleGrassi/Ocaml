(* Esercizio 2e *)
(* stringa vuota *)

let num_di_stringhe () = 
	let rec aux n =
		let s = read_line()
		in match s with
		| "." -> n
		| _ -> aux (n+1)
in aux 0