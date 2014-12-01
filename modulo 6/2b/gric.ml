(* Esercizio 2b *)
(* trova_colonna: 'a * (('b * 'c) * 'd) list -> 'b -> 'd -> 'c*)
(* è una funzione che dato un labirinto, un indice n ed *)
(* elemento x, ritorna l'indice c della colonna tale che (n,c)*)
(* contenga x*)
(* Questa funzione solleva l'eccezione Elemento_non_trovato *)
(* se non viene trovato l'elemento x nella riga n*)

exception Elemento_non_trovato

let trova_colonna labirinto n x = 
	try
		snd(fst(List.find (function y -> fst(fst y) = n && (snd y) = x)(snd labirinto)))
	with Not_found -> raise Elemento_non_trovato