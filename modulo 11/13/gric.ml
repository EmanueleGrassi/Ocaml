(* Esercizio 13 *)
(* path_n_p: 'a graph -> ('a -> bool) -> int -> 'a -> 'a list*)
(* è una funzione che dato un grafo g, un predicato p, un intero n ed un nodo start restituisce un*)
(* cammino in cui siano presenti n nodi che soddisfano p.*)
(* Se tale cammino non esiste viene sollevata l'eccezione NotFound.*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> int -> 'a -> 'a list e from_list: 'a list -> int ->'a list -> 'a list*)
(* from_node visited num a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a soddisfa p(diminuendo num di 1). Se num è 0*)
(* allora restituisce il cammino, altrimenti lancia la ricerca nei nodi adiacenti(chiamando from_list) *)
(* from_list visited num vicini è una funzione che chiama from_node visited num n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)

exception NotFound
type 'a graph = ('a * 'a) list

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest
			
let path_n_p graph p n start = 
	let rec from_node visited num a =
    if List.mem a visited 
    then raise NotFound
    else if p a 
		     then let num2 = num-1
				      in if num2 = 0 
							   then [a]
								 else a::from_list (a::visited) num2 (vicini a graph)
         else a::from_list (a::visited) num (vicini a graph)
  and from_list visited num = function
      [] -> raise NotFound
    | a::rest -> try from_node visited num a 
                 with NotFound -> from_list (a::visited) num rest
  in from_node [] n start