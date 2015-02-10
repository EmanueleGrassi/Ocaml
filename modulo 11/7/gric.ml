(* Esercizio 7 *)
(* colori_alterni: 'a graph -> 'a assoc -> 'a -> 'a -> 'a list *)
(* colori alterni dato un grafo g, una associazione di colori assoc, un nodo start ed un*)
(* nodo goal restituisce un cammino da start a goal i cui nodi hanno colori alterni.*)
(* La funzione solleva l'eccezione NotFound se tale cammino non esiste*)
(* Questa funzione utilizza colore: 'a -> 'a col_assoc -> col *)
(* colore è una funzione che dato un valore x ed una lista associativa*)
(* restituisce se presente il colore a cui è associata la lista contente x*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando viene raggiunto il nodo goal. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> 'a list e from_list: 'a list -> col -> 'a list -> 'a list*)
(* from_node visited a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* e se a è diverso da n, sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a è il nodo n*)
(* oppure lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* from_list visited lastcol vicini è una funzione che chiama from_node visited n, dove n *)
(* è un elemento della lista vicini solo se n ha un colore diverso da lastcol. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)

type 'a graph = ('a * 'a) list
exception NotFound
type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
exception Non_Presente

let rec colore x = function
	| [] -> raise Non_Presente
	| (col,lst)::rest -> if List.mem x lst
                       then col
											 else colore x rest 

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo then y::vicini nodo rest
      else vicini nodo rest
			
let colori_alterni graph assoc start goal =
    let rec from_node visited a =
      if List.mem a visited 
      then raise NotFound
      else if a = goal 
			     then [a]   
           else let col = colore a assoc
					      in a::from_list (a::visited) col (vicini a graph) 
    and from_list visited lastcol = function
        [] -> raise NotFound
      | a::rest -> try if colore a assoc <> lastcol
			                 then from_node visited a 
											 else raise NotFound
                   with NotFound -> from_list (a::visited) lastcol rest
    in from_node [] start