(* Esercizio 1l *)
(* interleave: 'a -> 'a list -> 'a list list *)
(* è una funzione che riporta una lista di liste che si ottiene inserendo l'elemento x*)
(* e in ogni posizione della lista parametro *)
(* ad ogni passaggio la lista viene ridotta fino al caso base, e viene aggiunto mano*)
(* mano l'elemento x*)

let rec interleave e = function
	| [] -> [[e]]
	| x::rest as lst -> (e::lst) :: (List.map (function y -> x::y) (interleave e rest))