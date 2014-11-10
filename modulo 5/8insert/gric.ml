(* Esercizio 8a*)
(* insert: 'a -> 'a list -> 'a list*)
(* sort: 'a list -> 'a list*)
(* sort è una funzione che ordina una lista secondo il metodo dell'inserimento*)
(* in ordine crescente*)
(* utilizza la funzione insert che dato un elemento ed una lista inserisce*)
(* l'elemento passato come parametro nella posizione corretta*)


let rec insert x = function
  | [] -> [ x ]
  | y::rest -> if x < y 
							 then x::y::rest 
							 else y::(insert x rest)
  
let rec sort = function
	| [] -> [] 
	| x :: rest -> insert x (sort rest)
  
