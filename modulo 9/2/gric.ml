(* Esercizio 2 *)
(* mkand: form list -> form *)
(* è una funzione che data una lista di formule riporta la congiunzione di*)
(* tutti gli elementi di tale lista *)
(* Se la lista è vuota la funzione riporta True *)

let rec mkand = function
	| [] -> True
	| x::y::rest -> And(x, mkand (y::rest))
	| x::rest -> x