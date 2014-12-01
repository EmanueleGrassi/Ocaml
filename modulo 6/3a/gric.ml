(* Esercizio 3a*)
(* find: 'a -> 'a list -> 'a list * 'a list*)
(* è una funzione che preso un elemento x ed una lista l*)
(* spezza l in due liste contenenti una la prima parte di l*)
(* fino alla prima occorrenza di x (escluso) e l'altra la seconda parte*)
(* e le restituisce come coppia *)
(* find utilizza la funzione aux*)
(* aux: ('a list * 'a list) -> 'a list -> ('a list * 'a list)*)
(* nella quale tmp è la lista risultato nella quale ad ogni passo*)
(* viene aggiunto un elemento(nel primo membro della coppia),*)
(* fino ad arrivare all'occorrenza di x, a quel punto viene messo*)
(* il resto della lista nel secondo membro della coppia.*)
(* find solleva l'eccezione Elemento_non_trovato se la lista*)
(* non contiene x*)

exception Elemento_non_trovato

let find x l =
	if not (List.mem x l)
	then raise Elemento_non_trovato
	else let rec aux tmp = function
		| [] -> tmp
		| y::rest -> if y = x
		             then (List.rev (fst tmp), rest)
								 else aux (y::(fst tmp), snd tmp) rest
		   in aux ([],[]) l
			
			
exception Empty_List

let rec index y = function
	|[]-> raise Empty_List
	|x::rest -> if(x = y) 
							then 0
							else 1 + index y rest

						
																		