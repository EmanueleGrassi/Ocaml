(* Esercizio 8 *)
(* connessi_in_glist: 'a graph list -> 'a -> 'a -> bool*)
(* è una funzione che data una lista di grafi glist e un nodo start e uno goal verifica*)
(* che esista almeno un grafo nella lista glist che contenga un cammino da start a goal*)
(* La funzione utilizza esiste_cammino: 'a graph -> 'a -> 'a -> bool che dato un grafo g*)
(* ed un nodo a e un nodo b, verifica l'esistenza di un cammino tra a e b in g. *)
(* esiste_cammino utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando viene raggiunto il nodo goal. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> bool e from_list: 'a list -> 'a list -> bool*)
(* from_node visited a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* e se a è diverso da n, sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a è il nodo n*)
(* oppure lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* from_list visited vicini è una funzione che chiama from_node visited n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)

type 'a graph = ('a * 'a) list
exception NotFound
						
let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest
			
let esiste_cammino graph start goal =
	let rec from_node visited a =
    if List.mem a visited 
    then raise NotFound
    else a = goal || from_list (a::visited) (vicini a graph)
  and from_list visited = function
      [] -> raise NotFound
    | a::rest -> try from_node visited a 
                 with NotFound -> from_list (a::visited) rest
  in try from_node [] start
	   with NotFound -> false	
			
let connessi_in_glist lgraph n m = 
	n <> m &&	List.exists (function x -> esiste_cammino x n m || esiste_cammino x m n) lgraph
	
