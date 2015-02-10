(* Esercizio 5*)
(* listaGuida: 'a list -> 'a ntree -> 'a*)
(* è una funzione che data una lista di interi non negativi l ed un albero n-ario t*)
(* ritorna l'etichetta del nodo ottenuto scendendo per ogni elemento n di l al *)
(* sottoalbero n-esimo di t*)
(* Questa funzione lancia l'eccezione Sottoalbero_non_esistente se il sottoalbero *)
(* non esiste *)

exception Sottoalbero_non_esistente

let rec listaGuida l (Tr(x,tlist)) = 
	match l with
	| [] -> raise Sottoalbero_non_esistente
	| [n] ->  (try (List.nth tlist n)
	          with Failure "nth" -> raise Sottoalbero_non_esistente)
	| n::rest -> (try listaGuida rest (List.nth tlist n)
	             with Failure "nth" -> raise Sottoalbero_non_esistente)