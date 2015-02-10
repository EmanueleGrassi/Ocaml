(* Esercizio 2a*)
(* reflect : 'a tree -> 'a tree*)
(* reflect è una funzione che dato un albero ritorna lo stesso albero in cui*)
(* ogni sottoalbero sinistro viene sostituito dal sottoalbero destro avente la*)
(* stessa altezza e viceversa *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec reflect = function 
	| Empty -> Empty
	| Tr(x,a,b) -> Tr(x,reflect b, reflect a)