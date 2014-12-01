(* Esercizio 2e *)
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

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
	                    in try Tr(List.hd(drop (n-1) (take n tmp)),balinorder (take (n-1) tmp), balinorder (drop n rest))
											   with Failure "hd" -> Empty
