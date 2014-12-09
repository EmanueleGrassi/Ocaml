(* Esercizio 15d*)
(* RICCARDO HA DETTO NO ALL'AUX *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception E

let rec abr_delmin = function
	| Empty -> raise E
	| Tr((k,v),Empty,_) -> ((k,v),Empty)
	| Tr(x,a,b) -> (fst (abr_delmin a),Tr(x,snd(abr_delmin a),b))