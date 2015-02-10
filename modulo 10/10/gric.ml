(* Esercizio 10 *)
(* path_non_pred: ('a -> bool) -> 'a ntree -> 'a list*)
(* è una funzione che dato un predicato p ed un albero t, ritorna una lista rappresentante*)
(* un cammino i cui elementi non soddisfano p. *)
(* Questa funzione solleva l'eccezione Cammino_non_esistente se tale cammino nono esiste*) 

exception Cammino_non_esistente

let rec path_non_pred p = function
	| Tr(x,[]) -> if p x then raise Cammino_non_esistente else [x]
	| Tr(x,tlist) -> if p x
	                 then raise Cammino_non_esistente
									 else (match tlist with
  									      | [] -> raise Cammino_non_esistente
  								      	| [y] -> x::path_non_pred p y
  												| y::rest -> try x::path_non_pred p y
  											               with Cammino_non_esistente -> path_non_pred p (Tr(x,rest)))