(* Esercizio 2g *)
(* alternate è una funzione che data una lista riporta una lista con *)
(* gli elementi in posizione dispari*)
(* alternate: 'a list -> 'a list*)
(* alternate utilizza la funzione aux: int -> 'a list -> 'a list -> 'a list *)
(* che utilizza la prima lista parametro per costruire la lista risultato*)
(* NB: nella funzione aux è stata utilizzata la funzione List.rev, ma avremmo*)
(* potuto usare anche la funzione reverse definita nell'esercizio 1f *)

let alternate xs =
	let rec aux n lst = function
		| [] -> List.rev lst
		| x::rest -> if n mod 2 = 0
								 then aux (n+1) lst rest
								 else aux (n+1) (x::lst) rest
	in aux 0 [] xs