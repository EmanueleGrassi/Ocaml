(* Esercizio 14*)
(* colore: 'a -> 'a col_assoc -> col *)
(* colore è una funzione che dato un valore x ed una lista associativa*)
(* restituisce se presente il colore a cui è associata la lista contente x*)
(* Questa funzione solleva l'eccezione Non_Presente se x non è associato a nessun*)
(* colore *)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

exception Non_Presente

let rec colore x = function
	| [] -> raise Non_Presente
	| (col,lst)::rest -> if List.mem x lst
                       then col
											 else colore x rest 
											
											
(* path_to: 'a -> 'a col_assoc -> 'a tree -> 'a list*)
(* è una funzione che dato un elemento x, una lista associativa l ed un albero t*)
(* riporta una lista rappresentante un cammino dalla radice alla foglia etichettata*)
(* da x, in cui ciascun nodo ha un colore differente dal nodo successivo.*)
(* e getinfo: 'a tree -> 'a, funzione che dato un nodo dell'albero restituisce*)
(* la sua etichetta*)
(* Questa funzione solleva l'eccezione Non_Esiste se questo cammino è inesistente *)

exception Non_Esiste

let getinfo = function
	| Empty -> failwith "impossibile"		 	
	| Tr(x,_,_) -> x								
									
let rec path_to x colori = function
	| Empty -> []
	| Tr(y,Empty,Empty) -> if x = y then [x] else raise Non_Esiste
	| Tr(y,a,Empty) -> if colore y colori = colore (getinfo a) colori
	                   then raise Non_Esiste
										 else y::(path_to x colori a)
	| Tr(y,Empty,b) -> if colore y colori = colore (getinfo b) colori
	                   then raise Non_Esiste
										 else y::(path_to x colori b)
	| Tr(y,a,b) -> try if colore y colori = colore (getinfo a) colori
	                   then raise Non_Esiste
										 else y::(path_to x colori a)
								 with Non_Esiste -> if colore y colori = colore (getinfo b) colori
																		then raise Non_Esiste
      										 					else y::(path_to x colori b)