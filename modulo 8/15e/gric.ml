(* Esercizio 15e *)
(* abr_delete: ('a * 'b) ntree -> 'a -> ('a * 'b) ntree*)
(* Questa funzione dato un albero binario di ricerca t ed una chiave a *)
(* ritorna l'albero da cui è stato rimosso il nodo la cui chiave è a.*)
(* La rimozione del nodo viene effettuata mantenendo l'albero un albero*)
(* binario di ricerca.*)
(* Questa funzione utilizza min: 'a ntree -> 'a che dato un albero ritorna*)
(* l'etichetta del nodo con chiave minore.*)
(* Questa funzione utilizza abr_delmin, definita nell'esercizio precedente*)
(* la funzione min e abr_delete sollevano l'eccezione Albero_Vuoto se *)
(* gli viene passato come parametro un albero vuoto *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception Albero_Vuoto

let rec min = function
	| Empty -> raise Albero_Vuoto
	| Tr(x,Empty,_) -> x
	| Tr(x,a,b) -> min a

let rec abr_delete tree x = 
	match tree with
	| Empty -> raise Albero_Vuoto
	| Tr((k,v),Empty,Empty) as leaf -> if k = x then Empty else leaf
	| Tr((k,v),a,Empty) -> if k = x then a else abr_delete a x
	| Tr((k,v),Empty,b) -> if k = x then b else abr_delete b x
	| Tr((k,v),a,b) -> if k = x
	                   then Tr(min b,a,snd (abr_delmin b))
										 else Tr((k,v),abr_delete a x, abr_delete b x)