(* Esercizio 15b *)
(* abr_search: ('a * 'b) ntree -> 'a -> 'b *)
(* è una funzione che dato un albero ed una chiave x restituisce*)
(* il valore associato ad x*)
(* Questa funzione solleva l'eccezione Non_Presente se tale chiave*)
(* non è presente nell'albero *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception Non_Presente

let rec abr_search tree x =
	match tree with
	| Empty -> raise Non_Presente
	| Tr((k,v),Empty,Empty) -> if k = x then v else raise Non_Presente
	| Tr((k,v),a,b) -> if x > k 
	                   then abr_search b x
										 else if x = k
										      then v
													else abr_search a x