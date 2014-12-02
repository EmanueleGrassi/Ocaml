(* Esercizio 5 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

exception Errore

let rec segui_bool lst = function
	| Empty -> raise Errore
	| Tr(x,a,b) -> match lst with
                | [] -> x
								| x::rest -> if x 
								             then segui_bool rest a
														 else segui_bool rest b 