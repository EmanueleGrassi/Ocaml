(* Esercizio 3 *)
(* foglie_in_lista: 'a list -> 'a ntree -> bool *)
(* è una funzione che data una lista l e un albero t verifica che tutte le*)
(* foglie di t siano contenute in l. Per far ciò si serve della funzione *)
(* treeleaf2list: 'a ntree -> 'a list*)
(* che genera una lista contenente tutte le foglie di un albero dato. Con questo*)
(* risultato foglie_in_lista ha semplicemente bisogno di verificare che per ogni elemento*)
(* di l si verifichi che l'elemento sia presente nel risultato di treeleaf2list*)

let rec treeleaf2list (Tr(x,tlist)) =
 List.flatten(List.map(function (Tr(x,trlist)) as albero -> 
	   if (trlist = []) 
     then [x] 
		 else (treeleaf2list albero)) tlist)

let rec foglie_in_lista l tree = 
	let listafoglie = treeleaf2list tree
	in List.for_all (function x -> List.mem x l) listafoglie