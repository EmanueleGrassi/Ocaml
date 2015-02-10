(* Esercizio 2 *)
(* postorder e inorder sono due funzioni del tipo*)
(* 'a ntree -> 'a list che implementano la visita in postordine e simmetrica*)
(* di alberi n-ari*)

let rec postorder (Tr(x,tlist)) =
  List.flatten (List.map postorder tlist)@[x]
  
let rec inorder (Tr(x, tlist)) =
	match tlist with
  | [] -> [x]
  | [y] -> [x] @ (inorder y)
  | y :: rest ->((inorder y)@[x])@(List.flatten (List.map inorder rest))
  
