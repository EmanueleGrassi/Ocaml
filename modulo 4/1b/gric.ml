(* Esercizio 1b *)
(* sumof è una funzione che calcola la somma di tutti gli elementi di una lista *)
(* sumof: 'a list -> int*)
(* sumof_coda è la versione ricorsiva di coda di sumof*)
(* sumof utilizza la funzione aux: int -> 'a list -> int*)
(* che utlizza un intero come accumulatore della somma degli elementi *)

let rec sumof = function 
	| [] -> 0
	| x::rest -> x + sumof rest

let sumof_coda l = 
	let rec aux f = function
		| [] -> f
		| x::rest -> aux (x+f) (rest)
	in aux 0 l