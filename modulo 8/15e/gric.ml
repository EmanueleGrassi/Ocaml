(* Esercizio 15e *)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree
exception E

let rec min = function
	| Empty -> failwith "albero vuoto"
	| Tr(x,Empty,_) -> x
	| Tr(x,a,b) -> min a

let rec abr_delmin = function
	| Empty -> raise E
	| Tr((k,v),Empty,_) -> ((k,v),Empty)
	| Tr(x,a,b) -> (fst (abr_delmin a),Tr(x,snd(abr_delmin a),b))

let rec abr_delete tree x = 
	match tree with
	| Empty -> failwith "vuoto?"
	| Tr((k,v),Empty,Empty) as leaf -> if k = x then Empty else leaf
	| Tr((k,v),a,Empty) -> if k = x then a else abr_delete a x
	| Tr((k,v),Empty,b) -> if k = x then b else abr_delete b x
	| Tr((k,v),a,b) -> if k = x
	                   then Tr(min b,a,snd (abr_delmin b))
										 else Tr((k,v),abr_delete a x, abr_delete b x)