(* Esercizio 5*)
(* segui_bool: bool list -> 'a tree -> 'a *)
(* segui_bool lst tree è una funzione che scorre tree a seconda dei valori di lst:*)
(* se trova true la ricerca si sposta a sinistra, altrimenti a destra *)
(* quando giunge alla fine di lst riporta l'etichetta del nodo in cui si ferma.*)
(* Questa funzione solleva l'eccezione Errore se l'altezza dell'albero è minore*)
(* della lunghezza della lista o se la ricerca si sposta su un nodo vuoto *)

exception Errore

let rec segui_bool lst = function
	| Empty -> raise Errore
	| Tr(x,a,b) -> match lst with
                | [] -> x
								| x::rest -> if x 
								             then segui_bool rest a
														 else segui_bool rest b 