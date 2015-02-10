(* Esercizio 2d *)
(* preorder, postorder, inorder: 'a tree -> 'a list*)
(* queste funzioni dato un albero restituiscono una lista in cui*)
(* sono presenti tutte le etichette dei nodi ordinate secondo il particolare*)
(* tipo di visita*)
(* preordine: nodo-sinistra-destra*)
(* postordine: sinistra-destra-nodo*)
(* inordine: sinistra-nodo-destra *)


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
