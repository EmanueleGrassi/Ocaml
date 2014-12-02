(* Esercizio 6 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let foglia_costo t =
	let rec aux (fst,snd) = function
  	| Empty -> (fst,snd)
  	| Tr(x,Empty,Empty) -> (x,x+snd)
  	| Tr(x,a,b) -> max (aux (fst,x+snd) a) (aux (fst, x+snd) b)
	in aux (0,0) t
