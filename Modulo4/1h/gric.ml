(* Esercizio 1h *)

let remove elem lst =
	let rec aux l = function
		| [] -> List.rev l
		| x::rest -> if x=elem
			 					 then aux l rest
								 else aux (x::l) rest
	in aux [] lst