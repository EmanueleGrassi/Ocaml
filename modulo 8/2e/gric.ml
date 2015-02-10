(* Esercizio 2e *)
(* balpreorder e balinorder: 'a list -> 'a tree *)
(* queste due funzioni data una lista restituiscono l'albero bilanciato *)
(* al quale se applicassimo preorder o inorder(rispettivamente a balpreorder e balinorder)*)
(* otterremmo la lista stessa *)
(* queste funzioni utilizzano drop e take, funzioni definite in precedenti homework *)


let rec drop n lst =
  try if n = 0 then lst else drop (n - 1) (List.tl lst)
  with Failure "tl" -> []
  
let rec take n =
  function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: (take (n - 1) xs)


let rec balpreorder = function
	| [] -> Empty
	| x::rest -> let n = (List.length rest)/2 
	             in Tr(x,balpreorder (take n rest), balpreorder (drop n rest))
							
let rec balinorder = function
	| [] -> Empty
	| x::rest as tmp -> let n = (List.length tmp)/2		
	                    in Tr((List.nth tmp n),balinorder (take n tmp), balinorder (drop n rest))
