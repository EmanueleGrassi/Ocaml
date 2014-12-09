(* Esercizio 14a *)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

exception E

let rec colore x = function
	| [] -> raise E
	| (col,lst)::rest -> if List.mem x lst
                       then col
											 else colore x rest
											
									
let rec path_to x colori = function
	| Empty -> []
	| Tr(y,Empty,Empty) -> if x = y then [x] else []
	| Tr(y,a,b) -> (match a with
	                | Empty -> []         
									| Tr(yy,Empty,Empty) ->
									| Tr(yy,aa,bb) ->  )
											
