(* Esercizio 2d *)
(* la funzione duplica è una funzione che presa un lista l restituisce una lista*)
(* che contiene ogni elemento 2 volte *)
(* duplica: 'a list -> 'a list *)
(* duplica utilizza aux: 'a list -> 'a list -> 'a list *)
(* che utilizza la prima lista parametro per costruire la lista risultato*)
(* NB: nella funzione aux è stata utilizzata la funzione List.rev, ma avremmo*)
(* potuto usare anche la funzione reverse definita nell'esercizio 1f *)

let duplica xs =
	let rec aux lst = function
		| [] -> List.rev lst
		| x::rest -> aux (x::x::lst) rest
	in aux [] xs