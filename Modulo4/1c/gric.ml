(* Esercizio 1c *)

exception Empty_List

let maxlist = function
	| [] -> raise Empty_List
	| x::rest -> let rec aux x rest = 
							 		match rest with
									| [] -> x
									| x1::rest1 -> aux (max x x1) rest1
						   in aux x rest	
		