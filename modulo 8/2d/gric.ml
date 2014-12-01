(* Esercizio 2d *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec preorder = function
	| Empty -> []
	| Tr(x,Empty,Empty) -> [x]
	| Tr(x,a,b) -> [x]@(preorder a)@(preorder b)

let rec postorder = function
	| Empty -> []
	| Tr(x,Empty,Empty) -> [x]
	| Tr(x,a,b) -> (postorder a)@(postorder b)@[x]

let rec inorder = function
	| Empty -> []
	| Tr(x,Empty,Empty) -> [x]
	| Tr(x,a,b) -> (inorder a)@[x]@(inorder b)
