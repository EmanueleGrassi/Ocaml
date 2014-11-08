(* Esercizio 2f *)
(* position è una funzione che dato un elemento ed una lista restituisce la posizione dell'elemento*)
(* nella lista. Se non viene trovato solleva l'eccezione Not_Found*)
(* position: 'a -> 'a list -> int *)
(* position utilizza la funzione aux: int -> 'a list -> int*)
(* che utilizza un intero per scandire la lista, quando viene trovato l'elemento *)
(* ricercato, questo intero sarà il risultato*)

exception Not_Found

let position elem xs =
	let rec aux n = function
	| [] -> raise Not_Found
	| x::rest -> if x = elem
							 then n
							 else aux (n+1) rest
  in aux 0 xs