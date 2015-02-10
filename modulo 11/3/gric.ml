(* Esercizio 3 *)
(* ciclo: 'a graph -> 'a -> 'a list*)
(* è una funzione che dato un grafo orientato g ed un nodo n riporta se esiste un ciclo su n.*)
(* La funzione solleva l'eccezione NotFound se tale ciclo non esiste.*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando viene raggiunto il nodo start. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> 'a list e from_list: 'a list -> 'a list -> 'a list*)
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

let ciclo graph nodo = 
	let rec from_node visited a =
    if List.mem a visited && a <> nodo
    then raise NotFound
    else if a = nodo
		     then [a]
				 else a::from_list (a::visited) (vicini a graph)
  and from_list visited = function
      [] -> raise NotFound
    | a::rest -> try from_node visited a 
                 with NotFound -> from_list (a::visited) rest
  in from_list [nodo] (vicini nodo graph) 