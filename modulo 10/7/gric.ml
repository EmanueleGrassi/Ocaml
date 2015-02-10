(* Esercizio 7 *)
(* tutte_foglie_costi: int ntree -> (int*int) list*)
(* Questa funzione dato un albero di interi t, restituisce una lista di coppie di interi*)
(* che rappresentano etichetta e costo delle foglie di t*)

let rec tutte_foglie_costi = function
	| Tr(x,[]) -> [(x,x)]
	| Tr(x,tlist) -> List.flatten( List.map (function l -> let res = tutte_foglie_costi l
              	                                         in List.map (function (f,c) -> (f,x+c)) res) tlist)