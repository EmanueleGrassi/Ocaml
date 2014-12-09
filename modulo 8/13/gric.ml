(* Esercizio 13 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
exception E

let remove_elem x l =
	let rec aux tmp = function
		| [] -> tmp
		| y::rest -> if y = x 
		             then tmp@rest
								 else aux (y::tmp) rest
	in aux [] l

let rec path_coprente tr l =
	match tr with 
  	| Empty -> raise E
  	| Tr(x,Empty,Empty) -> if (l=[] || l=[x]) then [x] else raise E
  	| Tr(x,a,b) -> if List.mem x l
  	               then let l2 = remove_elem x l
									      in (match a with
  									      | Empty -> x::(path_coprente b l2)
    								      | Tr(y,Empty,Empty) -> if (l=[] || l=[y]) 
  												                       then x::(path_coprente a l2) 
  																							 else x::(path_coprente b l2)
    											| Tr(y,aa,bb) -> try x::(path_coprente a l2)
  												                 with E -> x::(path_coprente b l2))
  								 else (match a with
									        | Empty -> x::(path_coprente b (remove_elem x l))
    								      | Tr(y,Empty,Empty) -> if (l=[] || l=[y]) 
  												                       then x::(path_coprente a (remove_elem y l)) 
  																							 else x::(path_coprente b l)
    											| Tr(y,aa,bb) -> try x::(path_coprente a l)
													                 with E -> x::(path_coprente b l))