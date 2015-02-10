(* Esercizio 9 *)
(* cammino_con_nodi: 'a graph -> 'a -> 'a list -> 'a list*)
(* è una funzione che dato un grafo g, un nodo start ed una lista di nodi l, restituisc un cammino*)
(* qualsiasi di g che contenga tutti i nodi presenti in l in qualsiasi ordine, partendo da start*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando la lista l è vuota. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a list -> 'a -> 'a list e from_list: 'a list -> 'a list -> 'a list -> 'a list*)
(* from_node visited tovisit a verifica se a è presente nella lista tovisit(lista dei nodi da visitare)*)
(* rimuovendolo in tal caso. Altrimenti lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* Quando la lista tovisit è vuota, viene restituito il cammino.*)
(* from_list visited tovisit vicini è una funzione che chiama from_node visited tovisit n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini. *)
(* La funzione remove: 'a -> 'a list -> 'a list*)
(* remove elem list è una funzione che dato un elemento ed una lista elimina l'elemento dalla lista, se presente*)

type 'a graph = ('a * 'a) list
exception NotFound

let rec remove elem = function
	| [] -> []
	| x::rest -> if x = elem
	             then rest
							 else x::remove elem rest
						
let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest

let cammino_con_nodi graph start lst =
	let rec from_node visited tovisit a =
    if List.mem a visited 
    then raise NotFound
    else let tovisit2 = remove a tovisit
		     in if tovisit2=[] 
		        then [a] 
            else a::from_list (a::visited) tovisit2 (vicini a graph)
  and from_list visited tovisit = function
      [] -> raise NotFound
    | a::rest -> try from_node visited tovisit a 
                 with NotFound -> from_list (a::visited) tovisit rest
  in from_node [] lst start
	
	
	
	
	
	
	
	
	
	
	
	
	
