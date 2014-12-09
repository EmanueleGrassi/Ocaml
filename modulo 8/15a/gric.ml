(* Esercizio 15a *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree

let getinfo tree = 
	match tree with 
	| Empty -> failwith "bad call"
	| Tr((k,v),_,_) -> k

let rec abr_check = function
	| Empty -> true
	| Tr((k,v),Empty,Empty) -> true
	| Tr((k,v),Empty,b) -> k < getinfo b && abr_check b 
	|	Tr((k,v),a,Empty) -> k < getinfo a && abr_check a
	| Tr((k,v),a,b) -> k < getinfo a && abr_check a &&
	                   k < getinfo b && abr_check b