(* Esercizio 4 *)
(* grafo_connesso: 'a graph -> bool*)
(* è una funzione che dato un grafo verica se esso è connesso.*)
(* La funzione utilizza la funzione vicini nodo grafo, definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* La funzione utilizza la funzione depth_first_collect, definita a lezione.*)
(* depth_first_collect: 'a graph -> 'a -> 'a list*)
(* che dato un grafo ed un nodo start effettua la visita in ampiezza del grafo a partire dal nodo b,*)
(* restituendo la lista di nodi visitati.*)

type 'a graph = ('a * 'a list) list
exception Nodo_inesistente

let vicini x grafo =
  try List.assoc x grafo
  with Not_found -> raise Nodo_inesistente

let grafo_connesso graph = 
	let start = fst(List.hd graph)
	in let depth_first_collect graph start =
  		 let rec search visited = function
            [] -> visited
          | n::rest -> 
                if List.mem n visited 
                then search visited rest 
                else search (n::visited) ((vicini n graph) @ rest)
       in search [] [start]
		in List.length(depth_first_collect graph start) = List.length (graph)