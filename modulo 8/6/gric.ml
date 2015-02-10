(* Esercizio 6 *)
(* foglia_costo: int tree -> (int * int) *)
(* questa funzione restituisce una coppia contente l'etichetta della foglia più costosa*)
(* e il suo costo.*)
(* Il costo di una foglia è la somma di tutte le etichette dei nodi del cammino che porta*)
(* dalla radice dell'albero alla foglia(compresa)*)

let foglia_costo t =
	let rec aux (fst,snd) = function
  	| Empty -> (fst,snd)
  	| Tr(x,Empty,Empty) -> (x,x+snd)
  	| Tr(x,a,b) -> max (aux (fst,x+snd) a) (aux (fst, x+snd) b)
	in aux (0,0) t
