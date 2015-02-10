(* Esercizio 13 *)
(* path_coprente: 'a tree -> 'a list -> 'a list*)
(* è una funzione che dato un albero t ed una lista l restituisce una lista*)
(* rappresentante un cammino dalla radice ad una foglia qualsiasi, nel quale siano *)
(* contenuti tutti le etichette presenti in l*)
(* Questa funzione usa remove_elem, funzione definita in un precedente homework*)
(* Questa funzione solleva l'eccezione Non_Esiste se tale cammino è inesistente*)

exception Non_Esiste

let remove_elem x l =
	let rec aux tmp = function
		| [] -> tmp
		| y::rest -> if y = x 
		             then tmp@rest
								 else aux (y::tmp) rest
	in aux [] l

let rec path_coprente tr l =
	match tr with 
  	| Empty -> raise Non_Esiste
  	| Tr(x,Empty,Empty) -> if l = [] || l = [x] then [x] else raise Non_Esiste
		| Tr(x,a,Empty) -> x::(path_coprente a (remove_elem x l))
		| Tr(x,Empty,b) -> x::(path_coprente b (remove_elem x l))
  	| Tr(x,a,b) -> try x::(path_coprente a (remove_elem x l))
		               with Non_Esiste -> x::(path_coprente b (remove_elem x l))