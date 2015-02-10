(* Esercizio 2c*)
(* balanced: 'a tree -> bool*)
(* balanced è una funzione che verifica che per ogni nodo dell'albero *)
(* i sottoalberi sinistro e destro abbiano altezze che differiscano al massimo *)
(* di uno. E' stata definita la funzione altezza: 'a tree -> int che*)
(* riporta l'altezza dell'albero passato come parametro *)

let rec altezza = function
	| Empty -> 0
	| Tr(x,a,b) -> 1 + (max (altezza a) (altezza b))

let rec balanced = function
	| Empty -> true
	| Tr(x,a,b) -> abs(altezza a - altezza b) <= 1 && balanced a && balanced b
