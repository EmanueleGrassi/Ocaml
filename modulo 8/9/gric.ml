(* Esercizio 9 *)
(* max_common_subtree: string tree -> string tree -> string tree *)
(* è una funzione che dati due alberi a b restituisce il massimo sottoalbero*)
(* comune ad a e b. *)
(* I nodi di tale sottoalbero a partire dalla radice avranno la stessa *)
(* etichetta che hanno i nodi corrispondenti ad a e b se essi sono uguali, altrimenti*)
(* con una foglia etichettata da "@"*)
(* aux: string tree -> string tree -> string tree *)
(* aux è la funzione che elabora il risultato. E' stata definita*)
(* per evitare i casi t1 o t2 = Empty *)

let max_common_subtree t1 t2 =
	if t1 = Empty || t2 = Empty 
  then Empty
	else let rec aux t1 = function
      		| Empty -> Empty
      		| Tr(tt2,c,d) -> (match t1 with 
				                    | Empty -> Tr("@", Empty, Empty)
														| Tr(tt1,a,b) -> if tt1 = tt2 
                        		                 then Tr(tt1, aux a c, aux b d)
                        										 else Tr("@", Empty, Empty))
			 in aux t1 t2