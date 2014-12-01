(* Esercizio 3 *)
(* giraChiave: chiave -> chiave*)
(* presa una chiave cambia il suo valore da aperta a chiusa o viceversa*)
(* giraPrima: chiave list -> chiave list*)
(* è una funzione che data una lista di chiavi, riporta la stessa lista*)
(* con la prima chiave girata*)
(* giraDopoChiusa: chiave list -> chiave list*)
(* è una funzione che data una lista di chiavi, cambia il valore della chiave successiva*)
(* alla prima chiave chiusa (dove possibile)*)
(* Se non è possibile trovare una chiave chiusa, o se la prima chiave chiusa incontrata*)
(* è l'ultima della lista allora la funzione solleva l'eccezione Operazione_non_eseguibile*)
(* successori: chiave list -> chiave list list*)
(* è una funzione che data una lista di chiavi l restituisce una lista di liste di chiavi*)
(* contenente massimo 2 elementi i quali sono rispettivamente il risultato della *)
(* funzione giraPrima applicata alla lista l e quello di giraDopoChiusa *)
(* applicata ad l. Se questa seconda funzione solleva un'eccezione allora*)
(* la lista conterrà solo il primo risultato *)

let giraChiave = function
	| Aperta -> Chiusa
	| Chiusa -> Aperta

let giraPrima = function
	| [] -> []
	| x::rest -> (giraChiave x)::rest

exception Operazione_non_eseguibile

let rec giraDopoChiusa = function
	| [] -> raise Operazione_non_eseguibile
	| x::rest -> if x = Chiusa
                  then if rest = [] 
											 then raise Operazione_non_eseguibile
											 else x::giraPrima rest
									else x::(giraDopoChiusa rest)

let successori clist = 
	let primoElem = giraPrima clist
	in try [primoElem; giraDopoChiusa clist]
     with Operazione_non_eseguibile -> [primoElem]
									