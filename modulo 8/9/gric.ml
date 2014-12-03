(* Esercizio 9 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let max_common_subtree t1 t2 =
	if t1 = Empty || t2 = Empty 
  then Empty
	else let rec aux t1 = function
      		| Empty -> Empty
      		| Tr(tt2,c,d) -> (match t1 with 
				                    | Empty -> Tr("@", Empty, Empty)
														| Tr(tt1,a,b) -> if tt1 = tt2 
                        		                 then Tr(tt1, aux a c, aux b d)
                        										 else Tr("@", Empty, Empty))
			 in aux t1 t2