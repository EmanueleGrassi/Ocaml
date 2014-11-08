(* Esercizio 2 *)

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