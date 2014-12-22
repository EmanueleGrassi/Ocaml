(* Esercizio 2 *)

type 'a ntree = Tr of 'a * 'a ntree list

let rec postorder (Tr(x,tlist)) =
    List.flatten (List.map postorder tlist)@x

let getinfo (Tr(x,tlist)) = x		
		
let rec inorder (Tr(x,tlist)) = 
	match tlist with
	| [] -> [x]
	| [y] -> (inorder y)::x
	| y::rest -> (inorder y)::x@List.flatten (List.map inorder rest)