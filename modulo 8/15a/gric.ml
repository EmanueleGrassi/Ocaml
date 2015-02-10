(* Esercizio 15a*)
(* abr_check: ('a * 'b) ntree -> bool*)
(* è una funzione che dato un albero ritorna true se è un albero binario di ricerca*)
(* ovvero se per ogni nodo dell'albero, la chiavi dell'etichetta è maggiore*)
(* di quella del figlio sinistro e minore di quello destro*)
(* Questa funzione utilizza getinfo: ('a * 'b) ntree -> 'a che è una funzione*)
(* che dato un nodo restituisce la chiave dell'etichetta *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree

let getinfo = function
	| Empty -> failwith "bad call"
	| Tr((k,v),_,_) -> k

let rec abr_check = function
	| Empty -> true
	| Tr((k,v),Empty,Empty) -> true
	| Tr((k,v),Empty,b) -> k < getinfo b && abr_check b 
	|	Tr((k,v),a,Empty) -> k < getinfo a && abr_check a
	| Tr((k,v),a,b) -> k < getinfo a && abr_check a &&
	                   k < getinfo b && abr_check b