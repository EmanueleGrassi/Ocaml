(* Esercizio 12 *)
(* applica: 'a sostituzione -> 'a tree -> 'a tree *)
(* applica è una funzione che data una sostituzione s ed un albero t*)
(* restituisce un albero dove i nodi x vengono sostituiti *)
(* dal valore associato ad x nella sostituzione *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
type 'a sostituzione = ('a * 'a tree) list

let rec gettree e = function
	| [] -> Empty
	| (x,lst)::rest -> if x = e 
	                   then lst 
										 else gettree e rest

let rec applica subst = function 
	| Empty -> Empty
	| Tr(x,Empty, Empty) -> gettree x subst
	| Tr(x,a,b) -> Tr(x,applica subst a,applica subst b)