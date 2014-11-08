(* Esercizio 1g *)
(* nth è una funzione che presi un intero n ed una lista l, restituisce *)
(* l'n-esimo elemento di l*)
(* se n è minore di 0 o maggiore della lunghezza della lista solleva l'eccezione *)
(* Argomento_non_Valido *)
(* nth: int -> 'a list -> 'a*)
(* nth usa la funzione aux: int -> 'a list -> 'a*)
(* aux utilizza un intero per controllare la scansione della lista *)

exception Argomento_non_Valido

let nth n lst =
	if n < 0 || n > (List.length lst)-1
	then raise Argomento_non_Valido
	else let rec aux m l = 
      		if m = n 
      		then List.hd l
      		else aux (m+1) (List.tl l)
			 in aux 0 lst