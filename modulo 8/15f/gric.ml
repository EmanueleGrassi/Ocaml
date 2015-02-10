(* Esercizio 15f*)
(* abr_insert: 'a tree -> 'a -> 'a tree*)
(* questa funzione dato un albero ed un'etichetta (chiave,valore)*)
(* ritorna l'albero in cui è stato aggiunto un nodo con tale etichetta *)
(* se è già presente un nodo la cui etichetta ha una chiave uguale a quella*)
(* del parametro allora viene effettuata l'aggiunta alla sinistra di questo nodo.*)
(* L'aggiunta del nodo viene effettuata mantenendo l'albero un albero*)
(* binario di ricerca. *)
(* treesort: 'a list -> 'a list*)
(* è una funzione che data una lista inserisce tutti gli elementi*)
(* della lista in un albero binario di ricerca e restituisce la lista*)
(* data dalla visita in ordine di tale albero *)
(* treesort utilizza la funzione aux: 'a tree -> 'a list -> 'a list*)
(* aux tmp l effettua abr_insert di tutti gli elementi di l in tmp*)
(* (costruendo nodo per nodo l'albero binario di ricerca) e*)
(* restituisce inorder tmp. *)
(* inorder è una funzione definita nell'esercizio 2d.*)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree

let rec abr_insert tree (a,b) =
	match tree with
	| Empty -> Tr((a,b),Empty,Empty)
	| Tr((k,v),Empty,Empty) -> if k = a 
	                           then Tr((k,v),Tr((a,b),Empty,Empty),Empty) 
														 else if a>k
														      then Tr((k,v),Empty,Tr((a,b),Empty,Empty))
																	else Tr((k,v),Tr((a,b),Empty,Empty),Empty)
	| Tr((k,v),aa,bb) -> if a > k
  	                   then Tr((k,v),aa,abr_insert bb (a,b))
  										 else if k = a
  										      then Tr((k,v),Tr((a,b),aa,Empty),bb)
  													else Tr((k,v),abr_insert aa (a,b),bb)
							
																												

let tree_sort l =
	let rec aux tmp = function
		| [] -> inorder tmp
		| x::rest -> aux (abr_insert tmp x) rest
	in aux Empty l