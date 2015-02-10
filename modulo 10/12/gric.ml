(* Esercizio 12 *)
(* ramo_colorato: 'a -> 'a col_assoc -> 'a ntree -> 'a list*)
(* è una funzione che dato un valore k, un associazione di colori assoc ed un albero t*)
(* ritorna una lista rappresentante un cammino i cui elementi adiacenti non hanno*)
(* mai lo stesso colore.*)
(* Questa funzione solleva l'eccezione Cammino_non_esistente se tale cammino nono esiste*) 
(* Questa funzione utilizza colore: 'a -> 'a col_assoc -> col *)
(* colore è una funzione che dato un valore x ed una lista associativa*)
(* restituisce se presente il colore a cui è associata la lista contente x*)
(* Questa funzione solleva l'eccezione Non_Presente se x non è associato a nessun colore*)
(* Questa funzione utilizza getinfo: 'a ntree -> 'a che dato un albero t*)
(* ritorna l'etichetta della radice*)

exception Non_Presente
exception Cammino_non_esistente

let rec colore x = function
	| [] -> raise Non_Presente
	| (col,lst)::rest -> if List.mem x lst
                       then col
											 else colore x rest 
											
let getinfo = function
	| Tr(x,_) -> x			
																			
let rec ramo_colorato k assoc = function
	| Tr(x,[]) -> if x = k then [x] else raise Cammino_non_esistente
	| Tr(x,tlist) -> let colnodo = colore x assoc
	                 in (match tlist with
  									      | [] -> raise Cammino_non_esistente
  								      	| [y] -> if colore (getinfo y) assoc <> colnodo
									               	 then x::(ramo_colorato k assoc y)
																   else raise Cammino_non_esistente
  												| y::rest -> try if colore (getinfo y) assoc <> colnodo
        									               	 then x::(ramo_colorato k assoc y)
        																   else raise Cammino_non_esistente
  											               with Cammino_non_esistente -> ramo_colorato k assoc (Tr(x,rest)))