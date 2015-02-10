(* Esercizio 11*)
(* same_structure: 'a ntree -> 'a ntree -> bool*)
(* è una funzione che dati due alberi t1 e t2 verica che questi hanno la stessa struttura,*)
(* ovvero se hanno la stessa altezza e se ogni livello di t1 ha lo stesso numero di nodi del*)
(* corrispettivo livello in t2*)

let rec same_structure t1 t2 =
	match t1 with
	| Tr(_,[]) -> (match t2 with
	               | Tr(_,[]) -> true
								 | _ -> false )
	| Tr(_,tlist) -> (match t2 with
	                  | Tr(_,[]) -> false
										| Tr(_,tlist2) -> try (List.for_all2 (fun x y -> same_structure x y) tlist tlist2)
										                  with Invalid_argument "List.for_all2" -> false )