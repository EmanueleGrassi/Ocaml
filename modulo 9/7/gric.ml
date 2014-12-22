(* Esercizio 7*)
(* and2list: form -> form list *)
(* è una funzione che data una congiunzione di formule f, restituisce una lista*)
(* contenente tutti gli atomi di tale congiunzione.*)
(* Se f non è una congiunzione di formule, allora la funzione solleva l'eccezione No_congiunzione*)

exception No_congiunzione

let rec and2list = function
	| Or(_,_) -> raise No_congiunzione
	| Imp(_,_) -> raise No_congiunzione
	| And(f,g) -> (and2list f)@(and2list g)
	| Not f -> (match f with
							| Prop s -> [Not f]
							| _ -> raise No_congiunzione )
	| r -> [r]