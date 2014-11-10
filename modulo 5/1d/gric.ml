(* Esercizio 1d *)
(* cancella: 'a -> ('a * 'b) list -> ('a * 'b) list *)
(* cancella è una funzione che elimina le coppie nelle quali il primo elemento*)
(* è uguale all'elemento passato come parametro*)
(* sono state fornite una versione ricorsiva ed una versione tail recursive*)
(* della stessa funzione.*)
(* La versione tail recursive utilizza la funzinone aux*) 
(* aux: ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list *)
(* nella quale tmp viene utilizzata per costruire il risultato finale *)

let cancella_coda elem lst =
	let rec aux tmp = function
		| [] -> List.rev tmp
		| (x,y)::rest -> if x = elem
	                   then aux tmp rest
										 else aux ((x,y)::tmp) rest
	in aux [] lst
	
let rec cancella elem = function
	| [] -> []
	| (x,y)::rest -> if x = elem
                   then cancella elem rest
									 else (x,y)::(cancella elem rest)