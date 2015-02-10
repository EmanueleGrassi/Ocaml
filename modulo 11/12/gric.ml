(* Esercizio 12 *)
(* non_contradictory_path: form graph -> form -> form -> form list*)
(* è una funzione che dato un grafo g ed un nodo start ed uno goal restituisce un cammino*)
(* da start a goal tale che al suo interno non ci siano formule contraddittorie*)
(* Se tale cammino non esiste la funzione solleva l'eccezione NotFound*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando si raggiunge il nodo goal. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a list -> 'a -> 'a list e from_list: 'a list -> 'a list ->'a list -> 'a list*)
(* from_node visited path a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* oppure se a è contraddittorio ad un qualunque elemento di path*)
(* sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a non è goal*)
(* lancia la ricerca nei nodi adiacenti(chiamando from_list) *)
(* from_list visited path vicini è una funzione che chiama from_node visited path n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)
(* La funzione complementare: form -> form è una funzione che data una formula restituisce *)
(* la sua complementare *)

type form =  Prop of string | Not of form
           | And of form * form | Or of form * form

exception NotFound
type 'a graph = ('a * 'a) list

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest

let rec complementare = function
	| Prop s -> Not(Prop s)
	| Not f -> f
	| And (f,g) -> And(complementare f, complementare g)
	| Or (f,g) -> Or(complementare f, complementare g)

let non_contradictory_path graph start goal =
	let rec from_node visited path a =
    if List.mem a visited || List.exists (function x -> a = complementare x) path
    then raise NotFound
    else if a = goal 
		     then List.rev(a::path) 
         else from_list (a::visited) (a::path) (vicini a graph)
  and from_list visited path = function
      [] -> raise NotFound
    | a::rest -> try from_node visited path a 
                 with NotFound -> from_list (a::visited) path rest
  in from_node [] [] start