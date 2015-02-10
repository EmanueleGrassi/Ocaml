(* Esercizio 1 *)
(* test_connessi: 'a graph -> 'a -> 'a -> bool*)
(* è una funzione che dato un grafo non orientato e due nodi start e goal restituisce *)
(* true se esiste un cammino tra start ed goal. *)
(* La funzione utilizza la funzione vicini nodo grafo, definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando viene raggiunto il nodo goal. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> bool e from_list: 'a list -> 'a list -> bool*)
(* from_node visited a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* e solleva l'eccezione NotFound in tal caso. Altrimenti controlla se a è il nodo goal*)
(* oppure lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* from_list visited vicini è una funzione che chiama from_node visited n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)

type 'a graph = ('a * 'a) list
exception NotFound

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo then y::vicini nodo rest
      else if y = nodo then x::vicini nodo rest
      else vicini nodo rest

let test_connessi graph start goal = 
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