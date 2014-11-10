(* Esercizio 1b *)
(* combine: 'a list -> 'b list -> ('a * 'b) list*)
(* è una funzione che date 2 liste restituisce una lista di coppie nella*)
(* quale la coppia n-esima è formata *)
(* dall'elemento n-esimo della prima e della seconda lista *)
(* se le liste hanno una lunghezza differente viene sollevata *)
(* l'eccezione Length_not_Equal *)
(* questa funzione utilizza aux*)
(* aux: ('a * 'b) list -> 'a list -> 'b list -> ('a*'b) list *)
(* nella quale ogni chiamata ricorsiva aggiunge le head delle due liste*)
(* (prese come coppia di valori) a tmp *)

exception Length_not_Equal

let combine lst1 lst2 = 
	if List.length lst1 <> List.length lst2
	then raise Length_not_Equal
	else let rec aux tmp lst1 lst2=
		try aux (((List.hd lst1), (List.hd lst2))::tmp) (List.tl lst1) (List.tl lst2)
		with Failure "tl" -> List.rev tmp
		   in aux [] lst1 lst2
	 

