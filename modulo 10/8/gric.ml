(* Esercizio 8 *)
(* ramo_da_lista: 'a ntree -> 'a list -> 'a -> 'a list*)
(* è una funzione che dato un albero t, una lista l, un valore k riporta un cammino dalla*)
(* radice alla foglia etichettata con k in cui tutti i nodi di tale cammino sono contenuti*)
(* nella lista l.*)
(* La funzione solleva l'eccezione Cammino_non_esistente se tale cammino non esiste*)
(* La funzione utilizza remove_elem: 'a -> 'a list -> 'a list *)
(* che dato un elemento e ed una lista l rimuove e da l *)

exception Cammino_non_esistente

let rec remove_elem e = function
	| [] -> []
	| x::rest -> if x = e
	             then rest
							 else x::(remove_elem e rest)

let rec ramo_da_lista tree l k = 
	match tree with
	| Tr(x,[]) -> if l = [] && x = k then [x] else raise Cammino_non_esistente
	| Tr(x,tlist) -> if List.mem x l
	                 then let res = List.flatten (List.map (function y -> let list = remove_elem x l
          									                                            in try x::(ramo_da_lista y list k)
          																															   with Cammino_non_esistente -> []) tlist)
												in if res = []
													 then raise Cammino_non_esistente
												   else res
									 else raise Cammino_non_esistente