(* Esercizio 11 *)
(* cammino_di_primi: int graph -> int -> int -> int list*)
(* è una funzione che dato un grafo g e due interi n ed m restituisce un cammina da n ad m di soli numeri primi*)
(* Se tale cammino non esiste la funzione solleva l'eccezione NotFound*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando si raggiunge il nodo m. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> 'a list e from_list: 'a list -> 'a list -> 'a list*)
(* from_node visited a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a è primo*)
(* ed in tal caso lancia la ricerca nei nodi adiacenti(chiamando from_list), altrimenti solleva l'eccezione*)
(* NotFound.*)
(* from_list visited vicini è una funzione che chiama from_node visited n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)
(* la funzione isPrime: int -> bool verifica che un intero sia un numero primo.*)

exception NotFound
type 'a graph = ('a * 'a) list

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest
	
let isPrime n =
  let rec noDivisors m =
    m * m > n || (n mod m != 0 && noDivisors (m + 1))
  in noDivisors 2			
				
let cammino_di_primi graph start goal =
	let rec from_node visited a =
      if List.mem a visited 
        then raise NotFound
        else if not(isPrime a)
				     then raise NotFound
						 else if a = goal then [a]
                  else a::from_list (a::visited) (vicini a graph)
      and from_list visited = function
        | [] -> raise NotFound
        | a::rest -> try from_node visited a 
                     with NotFound -> from_list (a::visited) rest
      in from_node [] start