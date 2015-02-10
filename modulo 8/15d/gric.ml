(* Esercizio 15d *)
(* abr_delmin: 'a ntree -> 'a * 'a ntree*)
(* questa funzione dato un albero binario di ricerca restituisce una coppia (a,b) dove*)
(* a è l'etichetta del nodo con la chiave minore e b è l'albero a cui è stato*)
(* eliminato tale nodo*)
(* Questa funzione solleva l'eccezione Albero_vuoto se ad abr_delmin viene*)
(* passato come parametro un albero vuoto *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception Albero_vuoto

let rec abr_delmin = function
	| Empty -> raise Albero_vuoto
	| Tr((k,v),Empty,_) -> ((k,v),Empty)
	| Tr(x,a,b) -> let tree = abr_delmin a
	               in (fst tree,Tr(x,snd tree,b))