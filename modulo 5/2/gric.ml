(* Esercizio 2 *)
(* le funzioni unione, intersezione e differenza sono funzioni*)
(* 'a list -> 'a list -> 'a list *)
(* che eseguono le omonime operazioni della teoria degli insiemi*)
(* sono state definite inoltre delle funzioni di supporto*)
(* cancella: 'a -> 'a list -> 'a list*)
(* che elimina ogni elemento della lista parametro uguale all'elemento*)
(* 'a passato come parametro*)
(* contenuto_in: 'a -> 'a list -> bool *)
(* che verifica se l'elemento parametro è presente nella lista parametro *)
(* la funzione subset*)
(* subset: 'a list -> 'a list -> bool *)
(* verifica che una lista sia un sottoinsieme (proprio o improprio)*)
(* di un'altra lista *)

let rec cancella elem = function
	| [] -> []
	| x::rest -> if x = elem
                   then cancella elem rest
									 else x::(cancella elem rest)	
	
																																					
let rec contenuto_in elem = function 
	| [] -> false
	| x::rest -> (elem = x) || (contenuto_in elem rest)


let rec unione lst = function
	| [] -> lst
	| x::rest -> unione (x::lst) rest		 
			
											
let rec intersezione lst = function 
	| [] -> []
	| x::rest -> if contenuto_in x lst
	             then x::intersezione lst rest
							 else intersezione lst rest
				
										
let rec differenza lst = function
	| [] -> lst
	| x::rest -> if contenuto_in x lst
	             then differenza (cancella x lst) rest
							 else differenza lst rest
			
											
let subset a b = b = intersezione a b || a = b