(* Esercizio 4*)
(* count: obj list -> int * int *)
(* è una funzione che data un obj list riporta una coppia di interi che rappresentano*)
(* il numero di missionari e quello di cannibali.*)
(* La funzione utilizza aux: int * int -> obj list -> int * int *)
(* che scandisce la lista passata come parametro ed aggiorna la coppia tmp quando*)
(* trova un missionario o un cannibale.*)
(* safe: situazione -> bool*)
(* safe è una funzione che data una situazione controlla se questa è sicura, ovvero*)
(* se i missionari sono in numero maggiore dei cannibali su entrambe le rive del fiume*)
(* remove_elem: è una funzione già definita in un precedente homework, e serve ad *)
(* eliminare un elemento da una lista.*)
(* remove_lst: 'a list -> 'a list -> 'a list*)
(* remove_lst a b è una funzione che elimina tutti gli elementi di a*)
(* dalla lista b utilizzando ad ogni passo ricorsivo la funzione remove_elem*)
(* sposta: situazione -> azione -> situazione*)
(* è una funzione ausiliaria ad applica. Il compito di questa funzione(che prende come *)
(* parametri una situazione sit ed una azione act) è spostare*)
(* a destra o a sinistra(a seconda di come specificato da act) gli elementi di act *)
(* applica: azione -> situazione -> situazione *)
(* applica act sit è una funzione che applica l'azione act alla situazione sit e*)
(* riporta il risultato di tale azione. Questa funzione può sollevare 2 eccezioni diverse.*)
(* Operazione_non_consentita se vogliamo spostare elementi da una riva sprovvista di *)
(* Barca, oppure se vogliamo spostare più Missionari o Cannibali di quanti ne abbiamo *)
(* disponibili.*)
(* Operazione_non_sicura: se applicando safe al risultato dell'azione viene*)
(* ritornato false*)
(* actions: azione list*)
(* è la lista di tutte le possibili azioni che possiamo eseguire*)
(* from_sit: situazione -> situazione list*)
(* è una funzione che data una situazione riporta una lista contenente tutte le possibili*)
(* situazioni ottenute applicando tutte le azioni della lista actions, escluse ovviamente*)
(* quelle non sicure o quelle che la funzione applica valuta come operazioni non consentite*)
(* from_sit utilizza la funzione aux: situazione list -> azione list -> situazione list *)
(* aux tmp l ad ogni iterazione aggiunge una situazione a tmp se la funzione applica*)
(* non solleva eccezioni*)

type obj = | Miss | Cann | Barca

type situazione = ((obj list) * (obj list))

type azione = | From_left of obj list 
              | From_right of obj list
							
let count l = 
	let rec aux (m,c) = function
	| [] -> (m,c)
	| x::rest -> match x with
	             | Miss -> aux (m+1,c) rest
							 | Cann -> aux (m,c+1) rest
							 | Barca -> aux (m,c) rest
	in aux (0,0) l

let safe (sinistra,destra) =
	let (msx,csx) = count sinistra
	in let (mdx, cdx) = count destra 
		 in ((msx >= csx) || (msx = 0 || csx = 0))
			  && ((mdx >= cdx) || (mdx = 0 || csx = 0))

let remove_elem x l =
	let rec aux tmp = function
		| [] -> tmp
		| y::rest -> if y = x 
		             then tmp@rest
								 else aux (y::tmp) rest
	in aux [] l
	
let rec remove_lst l lst =
	match l with
	| [] -> lst
	| x::rest -> remove_lst rest (remove_elem x lst)

let sposta (sx, dx) act =
	match act with
	| From_left a -> (List.filter ((<>)Barca) (remove_lst a sx), Barca::dx@a)
	| From_right b -> (Barca::sx@b, List.filter ((<>)Barca) (remove_lst b dx))		

exception Operazione_non_consentita
exception Operazione_non_sicura
		
let applica act (sx,dx) = 
	match act with
	| From_left a -> if not(List.mem (Barca) sx)  
	                 then raise Operazione_non_consentita
									 else let (msx, csx) = count sx
									      in let (mtomove, ctomove) = count a
												   in if msx >= mtomove && csx >= ctomove
													    then let sit = sposta (sx,dx) (From_left a)
															     in if safe sit
																	    then sit
																			else raise Operazione_non_sicura
															else raise Operazione_non_consentita
	| From_right b -> if not(List.mem (Barca) dx)  
	                  then raise Operazione_non_consentita 
										else let (mdx, cdx) = count dx
									       in let (mtomove, ctomove) = count b
												    in if mdx >= mtomove && cdx >= ctomove
													     then let sit = sposta (sx,dx) (From_right b)
															      in if safe sit
																	     then sit
																			 else raise Operazione_non_sicura
															 else raise Operazione_non_consentita
															
let actions =
  let elems =
    [[Miss];[Cann];[Miss;Cann];[Miss;Miss];[Cann;Cann]]
  in (List.map (function x -> From_left x) elems) 
	   @ (List.map (function x -> From_right x) elems)
		
let from_sit sit =
	let l = actions
  in let rec aux tmp = function
	| [] -> tmp
	| x::rest -> try aux ((applica x sit)::tmp) rest
	             with _ -> aux tmp rest
		 in aux [] l 
																																													
