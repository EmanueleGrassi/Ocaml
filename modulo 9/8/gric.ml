(* Esercizio 8 *)
(* satxand_of_lits: form -> bool*)
(* è una funzione che data una congiunzione di atomi verifica che per ogni*)
(* atomo all'interno della congiunzione non ci sia il suo complementare.*)
(* Se la congiunzione di atomi non è valida allora viene lanciata l'eccezione*)
(* No_congiunzione.*)
(* Questa funzione utilizza le funzioni and2list e complementare, definite negli esercizi precedenti*)


let satxand_of_lits f =
	let l = and2list f
  in List.for_all (function y -> not(List.exists 
				(function x -> List.mem (complementare x) l) l)) l