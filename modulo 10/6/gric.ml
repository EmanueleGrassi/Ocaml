(* Esercizio 6 *)
(* foglia_costo: int ntree -> (int * int) *)
(* questa funzione dato un albero t restituisce una coppia di interi che rappresentano*)
(* l'etichetta e il costo della foglia più costosa.*)
(* Il costo della foglia è calcolato come somma delle etichette del ramo*)
(* che parte dalla radice e arriva alla foglia stessa *)
(* La funzione utilizza massimo: (int*int) -> (int* int) list -> (int*int)*)
(* che presa una coppia come massimo temporaneo, ed una lista di coppie di interi*)
(* restituisce la coppia che ha il secondo membro più grande *)

let rec massimo (f,c) = function
	| [] -> (f,c)
	| (ff,cc)::rest -> if cc > c
	                   then massimo (ff,cc) rest
										 else massimo (f,c) rest

let rec foglia_costo = function
	| Tr(x,[]) -> (x,x)
	| Tr(x,tlist) -> let listacoppie = List.map (function l -> let (f,c) = foglia_costo l
	                                                           in (f,c+x) ) tlist
							     in massimo (List.nth listacoppie 0) listacoppie