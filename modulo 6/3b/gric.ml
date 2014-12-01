(* Esercizio 3b *)
(* spezza: 'a -> 'a list -> 'a list list*)
(* spezza è una funzione che utilizzando find(funzione definita*)
(* nell'esercizio 3a del modulo 6) spezza una lista in due sottoliste*)
(* a partire dalla seconda occorrenza di x escluso (elemento passato*)
(* come parametro) ignorando però tutto il contenuto della lista*)
(* precedente alla prima occorrenza di x (x incluso)*)
(* Questa funzione solleva l'eccezione Due_occorrenze_non_trovate*)
(* se nella lista non ci sono due occorrenze di x*)

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

exception Due_occorrenze_non_trovate

let spezza x l = 
	try
		find x (snd(find x l))
  with Elemento_non_trovato -> raise Due_occorrenze_non_trovate