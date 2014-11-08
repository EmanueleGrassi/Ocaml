(* Esercizio 1h *)
(* remove è una funzione che dato un elemento ed una lista, rimuove l'elemento *)
(* dalla lista, se presente *)
(* remove: 'a -> a' list -> a' list*)
(* remove utilizza aux: 'a list -> 'a list ->'a list*)
(* che utilizza la prima lista parametro per costruire la lista risultato*)
(* NB: nella funzione aux è stata utilizzata la funzione List.rev, ma avremmo*)
(* potuto usare anche la funzione reverse definita nell'esercizio 1f *)

let remove elem lst =
	let rec aux l = function
		| [] -> List.rev l
		| x::rest -> if x=elem
			 					 then aux l rest
								 else aux (x::l) rest
	in aux [] lst