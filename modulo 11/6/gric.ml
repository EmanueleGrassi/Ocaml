(* Esercizio 6 *)
(* cammino: 'a graph -> 'a list -> 'a -> 'a -> 'a list*)
(* hamiltoniano: 'a graph -> 'a list*)
(* La funzione cammino, dato un grafo g, una lista l, un nodo start ed un nodo goal restituisce un cammino*)
(* da start a goal i cui nodi siano presenti nella lista l.*)
(* la funzione hamiltoniano dato un grafo verifica che esista un ciclo che passi per tutti i nodi del grafo.*)
(* Le funzioni utilizzno la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usano anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando viene raggiunto il nodo goal. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> 'a list e from_list: 'a list -> 'a list -> 'a list*)
(* from_node tovisit a verifica se a non è presente nella lista tovisit(lista dei nodi da visitare)*)
(* sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a è il nodo goal*)
(* oppure lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* from_list tovisit vicini è una funzione che chiama from_node tovisit n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini. *)
(* La funzione remove: 'a -> 'a list -> 'a list*)
(* remove elem list è una funzione che dato un elemento ed una lista elimina l'elemento dalla lista, se presente*)

type 'a graph = 'a list * ('a * 'a) list
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

let cammino (nodi,archi) lst start goal = 
  let rec from_node tovisit a =
    if not(List.mem a lst)
		then raise NotFound
		else if a = goal
      	 then if (remove a tovisit) = [] then [a] else raise NotFound  
         else a::from_list (remove a tovisit) (vicini a archi)
  and from_list tovisit = function
    | [] -> raise NotFound
    | a::rest -> try from_node tovisit a 
                 with NotFound -> from_list (remove a tovisit) rest
  in from_node lst start
	
let hamiltoniano (nodi,archi) =
	let goal = List.hd nodi
  in let rec from_node tovisit a =
      if a = goal
  		then if (remove a tovisit) = [] then [a] else raise NotFound  
      else a::from_list (remove a tovisit) (vicini a archi)
    and from_list tovisit = function
      | [] -> raise NotFound
      | a::rest -> try from_node tovisit a 
                   with NotFound -> from_list (remove a tovisit) rest
    in from_list nodi (vicini goal archi)
