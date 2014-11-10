(* Esercizio 3 *)
(* explode: string -> char list*)
(* explode è una funzione che presa una stringa restituisce *)
(* una lista contenente i caratteri della stringa*)
(* explode utilizza la funzione aux*)
(* aux: char list -> int -> char list*)
(* nella quale la lista tmp è utilizzata per costruire il risultato e*)
(* l'intero n serve per scandire i caratteri della stringa*)

(* implode: char list -> string*)
(* implode è una funzione che data lista di caratteri*)
(* restituisce una stringa formata da tali caratteri *)
(* implode utilizza la funzione aux*)
(* aux: string -> char list -> string *)
(* ogni chiamata ricorsiva aggiunge alla stringa tmp un carattere preso *)
(* dalla lista di char passata come parametro *)

let explode str =
	let rec aux tmp = function
		| (-1) -> tmp
		| n -> aux (str.[n]::tmp) (n-1)
	in aux [] ((String.length str) -1)
	
let implode lst =
	let rec aux tmp = function
		| [] -> tmp
		| x::rest -> aux (tmp ^ (String.make 1 x) ) rest
	in aux "" lst