(* Esercizio 7 *)
(* foglie_costi: int tree -> (int * int) list *)
(* questa funzione ritorna una lista contentente tutte le coppie *)
(* (etichetta della foglia,costo della stessa) per ogni foglia dell'albero *)
(* NB: la definizione del costo di una foglia sta nell'esercizio precedente *)

let foglie_costi t =
	let rec aux lst tmp = function
  	| Empty -> lst
  	| Tr(x,Empty,Empty) -> (x,tmp+x)::lst
  	| Tr(x,a,b) -> (aux lst (tmp+x) a)@(aux lst (tmp+x) b)
	in aux [] 0 t