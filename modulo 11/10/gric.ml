(* Esercizio 10 *)
(* In questo esercizio si fa uso di funzioni definite nel gruppo di esercizi 7 esercizio 3*)
(* (giraChiave, giraPrima, successori,giraDopoChiusa) e della funzione tutte_liste_con definita*)
(* nel gruppo 6 esercizio 1k.*)
(* nodi: int -> cassaforte list*)
(* è una funzione che dato un intero n riporta tutte le possibili cassaforti con combinazioni*)
(* formate da n chiavi*)
(* archi: int -> (cassaforte * cassaforte list) list è una funzione che applicata ad un intero n*)
(* riporta per ogni possibile cassaforte(primo elemento della coppia) *)
(* con combinazioni di n chiavi, tutti i possibili successori(secondo elemento della coppia) *)
(* start: int -> cassaforte è una funzione che dato un intero n riporta una cassaforte*)
(* con combinazioni di n chiavi tutte chiuse.*)
(* aperta: cassaforte -> bool è una funzione che data una cassaforte verifica se essa è aperta*)
(* apri: int -> cassaforte list è una funzione che dato un intero n genera una cassaforte con*)
(* combinazione di n chiavi restituisce una lista che rappresenta la soluzione dell'apertura della*)
(* cassaforte*)
(* La funzione utilizza la funzione vicini nodo grafo(modificata per i grafi orientati), definita in classe, che*)
(* dato un nodo n ed un grafo, restituisce una lista contentente i nodi collegati al nodo n.*)
(* Usa anche la funzione di ricerca di un cammino, definita in classe, modificata in modo*)
(* da fermarsi quando aperta ritorna true. Questa funzione è composta*)
(* dalle funzioni from_node 'a list -> 'a -> 'a list e from_list: 'a list -> 'a list -> 'a list*)
(* from_node visited a verifica se a è presente nella lista visited(lista dei nodi visitati)*)
(* sollevando l'eccezione NotFound in tal caso. Altrimenti controlla se a è aperta*)
(* e in tal caso lancia la ricerca nei nodi adiacenti(chiamando from_list).*)
(* from_list visited vicini è una funzione che chiama from_node visited n, dove n *)
(* è un elemento della lista vicini. Se from_node lancia l'eccezione, questa viene*)
(* catturata e viene lanciata from_node con un altro nodo della lista vicini*)

exception NotFound
type chiave = Aperta | Chiusa
type cassaforte = chiave list

let rec vicini nodo = function 
  | [] -> []
  | (x,y)::rest ->
      if x = nodo 
			then y::vicini nodo rest
      else vicini nodo rest

let giraChiave = function
	| Aperta -> Chiusa
	| Chiusa -> Aperta

let giraPrima = function
	| [] -> []
	| x::rest -> (giraChiave x)::rest

exception Operazione_non_eseguibile

let rec giraDopoChiusa = function
	| [] -> raise Operazione_non_eseguibile
	| x::rest -> if x = Chiusa
                  then if rest = [] 
											 then raise Operazione_non_eseguibile
											 else x::giraPrima rest
									else x::(giraDopoChiusa rest)

let successori clist = 
	let primoElem = giraPrima clist
	in try [primoElem; giraDopoChiusa clist]
     with Operazione_non_eseguibile -> [primoElem]

let tutte_liste_con n a b =
	let rec aux tmp = function
		| 0 -> tmp
		| m -> aux ((List.map (function x -> (a::x)) tmp) @ (List.map (function x -> (b::x)) tmp)) (m-1)
	in aux [[]] n
	
	
let nodi n = tutte_liste_con n Aperta Chiusa

let archi n = 
	List.map (function x -> (x, successori x)) (nodi n)
	
let rec start = function
	| 0 -> []
	| x -> Chiusa::start (x-1)

let aperta cassaforte = List.for_all ((=) Aperta) cassaforte

let apri n =
	let start = start n
  in let rec from_node visited a =
      if List.mem a visited 
        then raise NotFound
        else if aperta a then [a]
             else a::from_list (a::visited) (successori a)
      and from_list visited = function
        | [] -> raise NotFound
        | a::rest -> try from_node visited a 
                     with NotFound -> from_list (a::visited) rest
      in from_node [] start
