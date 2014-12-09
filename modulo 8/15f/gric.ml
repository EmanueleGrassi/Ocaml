(* Esercizio 15f *)
(* treesort*)

type 'a ntree = Empty | Tr of 'a * 'a ntree * 'a ntree

let rec abr_insert tree (a,b) =
	match tree with
	| Empty -> Tr((a,b),Empty,Empty)
	| Tr((k,v),Empty,Empty) -> if k = a 
	                           then Tr((k,v),Tr((a,b),Empty,Empty),Empty) 
														 else if a>k
														      then Tr((k,v),Empty,Tr((a,b),Empty,Empty))
																	else Tr((k,v),Tr((a,b),Empty,Empty),Empty)
	| Tr((k,v),aa,bb) -> if a > k
  	                   then Tr((k,v),aa,abr_insert bb (a,b))
  										 else if k = a
  										      then Tr((k,v),Tr((a,b),aa,Empty),bb)
  													else Tr((k,v),abr_insert aa (a,b),bb)

let rec inorder = function
	| Empty -> []
	| Tr((k,v),Empty,Empty) -> [k]
	| Tr((k,v),a,b) -> (inorder a)@[k]@(inorder b)
																												
let tree_sort l =
	let rec aux tmp = function
		| [] -> inorder tmp
		| x::rest -> aux (abr_insert tmp (x,'a')) rest
	in aux Empty l