(* Esercizio 4 *)
(* intpairs : int -> (int * int) list *)
(* intpairs è una funzione che preso un intero restituisce una lista*)
(* con tutte le coppie (x,y) con x ed y compresi fra 1 ed n*)
(* intpairs utilizza aux*)
(* aux: (int * int) list  -> int -> int -> (int * int) list *)
(* che costruisce il risultato utilizzando la lista tmp.*)
(* ogni chiamata ricorsiva viene creata una nuova coppia (x,y) con y crescente*)
(* fino al valore n. A quel punto viene incrementato x, fino ad arrivare ad n*)
(* e quindi al risultato finale*)

let intpairs n = 
	let rec aux tmp m1 m2 =
		if m1 = (n+1)
		then List.rev tmp
		else if m2 = n
				 then aux ((m1,m2)::tmp) (m1+1) 1
		     else aux ((m1,m2)::tmp) m1 (m2+1)
	in aux [] 1 1