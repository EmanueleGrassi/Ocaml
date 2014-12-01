(* Esercizio 2a *)
(* in_riga: 'a * (('b * 'c) * 'd) list -> 'b -> 'd -> bool *)
(* è una funzione che dato un indice n ed un elemento x*)
(* ritorna true se nel labirinto passato come parametro *)
(* esiste alla riga n l'elemento x*)

let in_riga labirinto n x =
	 List.exists (function y -> fst(fst y) = n && (snd y) = x ) (snd labirinto)