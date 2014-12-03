(* Esercizio 10 *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec stessa_struttura t1 t2 = 
	match t1 with
	| Empty -> (match t2 with
	            | Empty -> true
							| _ -> false)
	| Tr(x,a,b) -> (match t2 with
	                | Empty -> false
									| Tr(xx,aa,bb) -> stessa_struttura a aa && stessa_struttura b bb)

let rec crea_lista t1 t2 = 
	match t1 with
	| Empty -> []
	| Tr(x,a,b) -> (match t2 with
	                | Empty ->  []
									| Tr(xx,aa,bb) -> if x = xx
									                  then (crea_lista a aa) @ (crea_lista b bb) 
																		else (x,xx)::(crea_lista a aa)@(crea_lista b bb))	

let esiste_mapping t1 t2 =
	if stessa_struttura t1 t2
	then let lista = crea_lista t1 t2
	     in List.for_all (function (x,y) -> not(List.exists (function (xx,yy) ->  (x = xx && y <> yy) || (x<>xx && y=yy)) lista) ) lista
	else false