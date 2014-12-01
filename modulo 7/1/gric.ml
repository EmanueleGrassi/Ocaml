(* Esercizio 1 *)
(* esegui: int * int * direzione -> azione list -> int * int * direzione*)
(* esegui è una funzione che data una posizione ed una lista di *)
(* azioni, restituisce la posizione dopo aver eseguito tutti i comandi*)
(* (le 'azioni') contenute nella lista *)
(* In questa funzione viene utilizzata 'sposta' definita a lezione*)

let rec esegui dir = function 
	| [] -> dir
	| x::rest -> esegui (sposta dir x) rest