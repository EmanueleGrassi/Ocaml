(* Esercizio 2c *)
(* in_tutte: 'a * ('b * 'c) list -> 'c -> bool *)
(* in_tutte è una funzione che verifica che tutte le caselle*)
(* del labirinto contengano l'elemento x passato come parametro*)

let in_tutte labirinto x =
	List.for_all (function y -> (snd y) = x) (snd labirinto)