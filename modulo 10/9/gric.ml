(* Esercizio 9 *)
(* ramo_di_primi: 'a ntree -> 'a*)
(* è una funzione che dato un albero t, restituisce l'etichetta di una foglia*)
(* facente parte di un cammino formato da etichette di soli numeri primi.*)
(* Questa funzione solleva l'eccezione Cammino_non_esistente se tale cammino non esiste*)
(* Questa funzione utilizza isPrime: int -> bool*)
(* per determinare se un intero è un numero primo o meno*)

exception Cammino_non_esistente

let isPrime n =
  let rec noDivisors m =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in noDivisors 2
																
let rec ramo_di_primi = function 
	| Tr(x,[]) -> if isPrime x then x else raise Cammino_non_esistente
	| Tr(x,tlist) -> if isPrime x
	                 then (match tlist with
									      | [] -> raise Cammino_non_esistente
								      	| [y] -> ramo_di_primi y
												| y::rest -> try ramo_di_primi y
											               with Cammino_non_esistente -> ramo_di_primi (Tr(x,rest)))
									 else raise Cammino_non_esistente