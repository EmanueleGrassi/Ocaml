(* Esercizio 15b *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception E

let rec abr_search tree x =
	match tree with
	| Empty -> raise E
	| Tr((k,v),Empty,Empty) -> if k = x then v else raise E
	| Tr((k,v),a,b) -> if x > k 
	                   then abr_search b x
										 else if x = k
										      then v
													else abr_search a x