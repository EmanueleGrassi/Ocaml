(* Esercizio 15c *)
(* abr_update: ('a * 'b) ntree -> ('a * 'b) -> 'a ntree *)
(* Questa funzione dato un albero t ed una coppia (chiave,valore) *)
(* aggiunge un nodo all'albero etichettato con tale coppia(facendo rimanere*)
(* l'albero un albero binario di ricerca). Se la chiave è già presente nell'albero*)
(* allora viene semplicemente aggiornato il suo valore associato *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree

let rec abr_update tree (a,b) =
	match tree with
	| Empty -> Tr((a,b),Empty,Empty)
	| Tr((k,v),Empty,Empty) -> if k = a 
	                           then Tr((k,b),Empty,Empty) 
														 else if a>k
														      then Tr((k,v),Empty,Tr((a,b),Empty,Empty))
																	else Tr((k,v),Tr((a,b),Empty,Empty),Empty)
	| Tr((k,v),aa,bb) -> if a > k
  	                   then Tr((k,v),aa,abr_update bb (a,b))
  										 else if k = a
  										      then Tr((k,b),aa,bb)
  													else Tr((k,v),abr_update aa (a,b),bb)