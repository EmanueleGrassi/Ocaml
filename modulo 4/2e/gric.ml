(* Esercizio 2e *)
(* la funzione enumera prende una lista come parametro e restituisce una*)
(* lista di coppie (n, elem) nella quale n è la posizione dell'elemento elem*)
(* enumera: 'a list -> (int*'a) list *)
(* enumera utilizza la funzione *)
(* aux: int -> (int * 'a) list -> 'a list -> (int * 'a) list *)
(* che utilizza la prima lista parametro per costruire la lista risultato *)
(* NB: nella funzione aux è stata utilizzata la funzione List.rev, ma avremmo*)
(* potuto usare anche la funzione reverse definita nell'esercizio 1f *)

let enumera xs =
	let rec aux y lst = function
		| [] -> List.rev lst
		| x::rest -> aux (y+1) ((y,x)::lst) rest
	in aux 0 [] xs