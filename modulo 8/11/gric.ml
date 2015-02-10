(* Esercizio 11*)
(* path: ('a -> bool) -> 'a tree -> 'a list *)
(* path p t è una funzione che dato un albero t restituisce un cammino dalla*)
(* radice ad una foglia qualsiasi in cui ciascun nodo non soddisfa il predicato p*)
(* La funzione solleva l'eccezione Non_Esiste se tale cammino non esiste *)

exception Non_Esiste

let rec path p = function
	| Empty -> raise E
	| Tr(x,Empty,Empty) -> if p x then [] else [x]
	| Tr(x,a,b) -> if p x
	               then raise Non_Esiste
								 else (match a with
								      | Empty -> x::(path p b)
								      | Tr(y,Empty,Empty) -> if p y then x::(path p b) else x::(path p a)
											| Tr(y,aa,bb) -> try x::(path p a)
											                 with Non_Esiste -> x::(path p b))