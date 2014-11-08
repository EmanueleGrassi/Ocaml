(* Esercizio 1d *)

(* cancella: 'a -> ('a * 'b) list -> ('a * 'b) list *)

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