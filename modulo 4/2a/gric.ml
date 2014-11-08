(* Esercizio 2a*)
(* copy è una funzione che dato un intero n ed un elemento, riporta una lista*)
(* di lunghezza n con elementi tutti uguali all'elemento parametro*)
(* copy: int-> 'a -> 'a list*)
(* copy utilizza aux: 'a list -> int -> 'a list *)
(* che utilizza la prima lista parametro per costruire la lista risultato*)

let copy n elem =
	let rec aux lst = function
		| 0 -> lst
		| n -> aux (elem::lst) (n-1)
	in aux [] n