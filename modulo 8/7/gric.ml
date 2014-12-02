(* Esercizio 7 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let foglie_costi t =
	let rec aux lst tmp = function
  	| Empty -> lst
  	| Tr(x,Empty,Empty) -> (x,tmp+x)::lst
  	| Tr(x,a,b) -> (aux lst (tmp+x) a)@(aux lst (tmp+x) b)
	in aux [] 0 t