(* Esercizio 3 *)
(* mkor: form list -> form *)
(* è una funzione che data una lista di formule riporta la disgiunzione di*)
(* tutti gli elementi di tale lista.*)
(* Se la lista è vuota, questa funzione ritorna False *)

let rec mkor = function
	| [] -> False
	| x::y::rest -> Or(x, mkor (y::rest))
	| x::rest -> x