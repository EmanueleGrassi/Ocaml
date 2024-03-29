(* Esercizio 2*)
(* esiste_ciclo: 'a graph -> 'a -> bool *)
(* � una funzione che dato un grafo orientato e un nodo start restituisce *)
(* true se esiste un ciclo su tale nodo *)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando viene raggiunto il nodo start. Questa funzione � composta*)
(* dalle funzioni from_node 'a list -> 'a -> bool e from_list: 'a list -> 'a list -> bool*)
(* from_node visited a verifica se a � presente nella lista visited(lista dei nodi visitati)*)
(* e se a � diverso da start, sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a � il nodo start*)
(* oppure lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* from_list visited vicini � una funzione che chiama from_node visited n, dove n *)
(* � un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)

type 'a graph = ('a * 'a) list
exception NotFound

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest
			
			
let esiste_ciclo graph nodo = 
	let rec from_node visited a =
    if List.mem a visited && a <> nodo
    then raise NotFound
    else a = nodo || from_list (a::visited) (vicini a graph)
  and from_list visited = function
      [] -> raise NotFound
    | a::rest -> try from_node visited a 
                 with NotFound -> from_list (a::visited) rest
  in try from_list [nodo] (vicini nodo graph)
	   with NotFound -> false 