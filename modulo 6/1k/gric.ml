(* Esercizio 1k *)
(* tutte_liste_con: int -> 'a -> 'a -> 'a list list *)
(* questa funzione riporta una lista contenente tutte le possibili liste*)
(* contenenti le permutazioni di lunghezza n(parametro) di due elementi a b*)
(* (anch'essi parametri)*)
(* La funzione utilizza aux *)
(* aux: 'a list list -> int -> 'a list list*)
(* che costruisce il risultato concatenando i risultati di List.map con la*)
(* quale andiamo a costruire le permutazioni possibili *)

let tutte_liste_con n a b =
	let rec aux tmp = function
		| 0 -> tmp
		| m -> aux ((List.map (function x -> (a::x)) tmp) @ (List.map (function x -> (b::x)) tmp)) (m-1)
	in aux [[]] n
	