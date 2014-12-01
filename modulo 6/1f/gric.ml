(* Esercizio 1f *)
(* verifica_matrice: int -> int list list -> bool*)
(* è una funzione che data una matrice di interi ritorna true*)
(* se almeno una riga contiene elementi tutti minori di n, *)
(* false altrimenti.*)
(* La definizione di m = n+0, serve per imporre alla funzione il tipo*)
(* int, altrimenti avremmo avuto una funzione 'a ->'a list list -> bool*)
(* dato che il predicato di List.for_all può essere applicato anche *)
(* ad altri tipi(ad esempio char) *)

let verifica_matrice n lst = 
	let m = n+0 
	in List.exists (List.for_all ((>) (m)) ) lst