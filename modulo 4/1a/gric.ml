(* Esercizio 1a *)
(* length è una funzione che calcola la lunghezza di un lista*)
(* length: 'a list -> int *)
(* length_coda è la stessa funzione definita con la ricorsione di coda *)
(* aux: int -> 'a list -> int *)
(* aux usa un intero n come accumulatore della lunghezza della lista *)


let rec length = function 
	| [] -> 0 
	| x :: rest -> 1 + (length rest)
  
let length_coda l =
  let rec aux n = function 
		| [] -> n
		| x :: rest -> aux (n + 1) rest
  in aux 0 l
  
