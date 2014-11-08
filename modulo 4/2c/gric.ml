(* Esercizio 2c *)
(* la funzione pairwith è una funzione che preso un elemento y ed una lista xs*)
(* riporta una lista di coppie formate da (y, elemento di xs)*)
(* pairwith: 'a -> 'b list -> ('a*'b) list *)
(* pairwith utilizza di aux: ('a*'b) list -> 'b list -> ('a*'b) list *)
(* che utilizza la prima lista parametro per costruire la lista risultato*)
(* NB: nella funzione aux è stata utilizzata la funzione List.rev, ma avremmo*)
(* potuto usare anche la funzione reverse definita nell'esercizio 1f *)

let pairwith y xs =
	let rec aux lst = function
		| [] -> List.rev lst
		| x::rest -> aux ((y,x)::lst) rest
	in aux [] xs