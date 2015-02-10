(* Esercizio 10b *)
(* esiste_mapping: 'a tree -> 'a tree -> bool *)
(* esiste_mapping t1 t2 è una funzione che dati due alberi verifica *)
(* che abbiano la stessa struttura *)
(* e che le coppie (x,y) riportate da crea_lista t1 t2 rappresentino*)
(* una funzione da x a y, ovvero trasforma l'albero t1 nell'albero t2 *)
(* crea_lista: 'a tree -> 'a tree -> ('a * 'a) list*)
(* crea_lista t1 t2 è una funzione che riporta una lista di coppie (x,y) *)
(* con x ed y rispettivamente etichette di nodi corrispondenti *)
(* in t1 e t2(questi due alberi devono avere la stessa struttura) *)

let rec crea_lista t1 t2 = 
	match t1 with
	| Empty -> []
	| Tr(x,a,b) -> (match t2 with
	                | Empty ->  []
									| Tr(xx,aa,bb) -> (x,xx)::(crea_lista a aa)@(crea_lista b bb))	

let esiste_mapping t1 t2 = stessa_struttura t1 t2 && 
		 let lista = crea_lista t1 t2
     in (List.for_all (function (x,y) -> 
  		not(List.exists (function (xx,yy) ->  (x = xx && y <> yy)) lista) )) lista