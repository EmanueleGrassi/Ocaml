(* Esercizio 2d *)

let occorre n = 
	let rec aux v = 
		try let b = int_of_string(read_line())
				in aux ((b=n) || v)
	  with _ -> v
	in aux false