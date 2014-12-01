(* Esercizio 4 *)
(* prendi: ('a -> bool) -> 'a list -> 'a * 'a list*)
(* è una funzione che dato un predicato p ed una lista l*)
(* restituisce una coppia contenente come primo membro, il primo*)
(* elemento x della lista che soddisfa p, come secondo membro tutta*)
(* la lista l eccetto la prima occorrenza dell'elemento x*)
(* Questa funzione solleva l'eccezione Predicato_non_soddisfatto*)
(* se nessun elemento della lista soddisfa p.*)
(* Per questa funzione è stata definita remove*)
(* remove: 'a -> 'a list -> 'a list*)
(* che preso un elemento x ed una lista l rimuove la prima occorrenza*)
(* di x in l.*)
(* la funzione utilizza aux*)
(* aux: 'a list -> 'a list -> 'a list*)
(* nella quale tmp serve per costruire la lista finale. Ad ogni passo*)
(* ricorsivo viene aggiunto un elemento, finchè non viene trovato*)
(* x. A quel punto viene aggiunto il resto della lista a tmp*)

let remove x l =
	let rec aux tmp = function
		| [] -> tmp
		| y::rest -> if y = x 
		             then tmp@rest
								 else aux (y::tmp) rest
	in aux [] l

exception Predicato_non_soddisfatto

let prendi p l =
	if not (List.exists p l)
	then raise Predicato_non_soddisfatto
	else let x = List.find p l
	     in (x, remove x l)