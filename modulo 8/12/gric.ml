(* Esercizio 12 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
type 'a sostituzione = ('a * 'a tree) list

let rec gettree e = function
	| [] -> Empty
	| (x,lst)::rest -> if x = e 
	                   then lst 
										 else gettree e rest

let rec applica subst = function 
	| Empty -> Empty
	| Tr(x,Empty, Empty) -> gettree x subst
	| Tr(x,a,b) -> Tr(x,applica subst a,applica subst b)