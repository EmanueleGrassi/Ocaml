(*** GRAFI ***)

(* Problema: attraversamento di un labirinto, da una casella
   di entrata a una casella di uscita, senza passare per caselle
   che contengono un mostro, e raccogliendo tutti gli oggetti che
   si trovano nelle altre caselle (se ce ne sono) *)

(* Rappresentazione:
   collegamento tra caselle: lista "associativa", ad ogni casella
   e` associata la lista delle caselle ad essa collegate *)
(** rappresentazione di un grafo mediante liste di successori **)
type 'a graph1 = ('a * 'a list) list

let grafo =
  [(1,[2;3;4]);(2,[3;4;1;7]);(3,[2;1;5]);(4,[6;7]);(5,[3;7]);
   (6,[4;7]);(7,[2;4;5;6])]
        (* 
                  1
                / | \
               3--2--4
               |   \/ \
               5---7---6
         *)
(* Problema di base nei grafi: dato un nodo e un grafo, 
   trovare i nodi vicini al nodo dato.  Se il nodo non \`e nel grafo, sollevare 
    un'eccezione *)
exception Nodo_inesistente
(* vicini_1: 'a  -> 'a graph1 -> 'a list *)
let vicini_1 x grafo =
  try List.assoc x grafo
  with Not_found -> raise Nodo_inesistente

(** Rappresentazione alternativa: mediante lista di archi (ed eventualmente
    insieme dei nodi).  La rappresentazione e` la stessa per grafi orientati
    e non orientati; la differenza tra i due tipi di grafo sar\`a nella 
    definizione della funzione successori **)
type 'a graph = ('a * 'a) list
let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)]
(* nel caso del labirinto gli archi non sono orientati *)

(* successori: se viene utilizzata per la visita di un grafo a partire
   da un nodo dato, non e` necessario controllare se i nodi appartengono
   al grafo o no: si partira' dal nodo iniziale seguendo i successori *)

(** successori in un grafo orientato **)
(* successori : 'a -> 'a graph -> 'a list  *)
let rec successori nodo = function 
      [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest 
(* oppure *)
let successori nodo grafo =
    List.map snd (List.filter (function (x,y) -> x=nodo) grafo)

(** vicini in un grafo non orientato **)
(*  vicini : 'a -> 'a graph -> 'a list  *)
let rec vicini nodo = function 
    [] -> []
  | (x,y)::rest ->
      if x = nodo then y::vicini nodo rest
      else if y = nodo then x::vicini nodo rest
      else vicini nodo rest

(** se serve ottenere la lista dei nodi (non isolati): **)
(* setadd: 'a -> 'a list -> 'a list *)
let setadd x set = if List.mem x set then set else x::set

(* nodes : 'a graph -> 'a list *)
let rec nodes = function
    [] -> []
  | (x,y)::rest ->
      setadd x (setadd y (nodes rest))

(*********** ALGORITMI DI BASE SUI GRAFI *************)

(** VISITA DI GRAFI: in profondita` e in ampiezza **)
(** attenzione ai cicli! **)
(* in profondita`: i nodi in attesa di essere visitati sono
                   gestiti come una pila
   in ampiezza: i nodi in attesa di essere visitati sono
                gestiti come una coda
Visita per collezionare i nodi visitati (grafo orientato): *)

(** visita in profondita` (grafo orientato) **)
(* depth_first_collect : 'a graph -> 'a -> 'a list *)
let depth_first_collect graph start =
    let rec search visited = function
        [] -> visited
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
            else search (n::visited) 
                        ((successori n graph) @ rest)
                  (** oppure: vicini n graph se il grafo non e' orientato **)
                            (* i nuovi nodi sono inseriti
                               in testa *)
    in search [] [start]

(** Verificare se tutti i nodi raggiungibili da start soddisfano un
    predicato p.  (grafo orientato) **)
(* conviene utilizzare List.for_all applicata al risultato di
   depth_first_collect? *)
(* depth_first_all: 'a graph -> 'a -> ('a -> bool) -> bool *)
let depth_first_all graph start p =
    let rec search visited = function
        [] -> true
      | n::rest -> (* i nodi visitati sono gia` stati controllati *)
            List.mem n visited && search visited rest 
	||  p n && search (n::visited) ((successori n graph) @ rest)
                  (** oppure: vicini n graph se il grafo non e' orientato **)
    in search [] [start]

(** visita in ampiezza (grafo orientato) **)
(* breadth_first_collect : 'a graph -> 'a -> 'a list *)
let breadth_first_collect graph start =
    let rec search visited = function
        [] -> visited
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
            else search (n::visited) 
                        (rest @ (successori n graph))
                  (** oppure: vicini n graph se il grafo non e' orientato **)
                            (* i nuovi nodi sono inseriti
                               in coda *)
    in search [] [start]

(** Cercare se dal nodo start e' raggiungibile un nodo che
    soddisfa il predicato p. Riportare tale nodo, se la ricerca
    ha successo (grafo orientato) **)
(* breadth_first: 'a graph -> 'a -> ('a -> bool) -> 'a *)
let breath_first graph start p =
    let rec search visited = function
        [] -> raise Not_found
      | n::rest -> 
            if List.mem n visited 
            then search visited rest 
	    else if p n then n
                 else search (n::visited) 
                             (rest @ (successori n graph))
                  (** oppure: vicini n graph se il grafo non e' orientato **)
    in search [] [start]


(** Quali sono i vantaggi e gli svantaggi dei due algoritmi di visita? **)

(** RICERCA DI UN CAMMINO mediante backtracking **)
(* versione generale: ricerca di un cammino in un grafo, a partire
    dal nodo start fino a un nodo che soddisfa il predicato p.
    Viene riportata una lista che rappresenta il cammino, o 
    sollevata un'eccezione (grafo non orientato) *)

exception NotFound

(* search_path : 'a graph -> 'a -> ('a -> bool) -> 'a list *)
let search_path graph start p =
   (* ricerca a partire da un singolo nodo *)
  let rec from_node visited a =
    if List.mem a visited 
    then raise NotFound
    else if p a then [a]   (* il cammino e' trovato *)
         else a::from_list (a::visited) (vicini a graph)
   (* ricerca a partire da una lista di nodi, tutti vicini di uno
      stesso nodo *)
  and from_list visited = function
      [] -> raise NotFound
    | a::rest -> (* provo a passare per a, ma se fallisco
                    cerco ancora passando per una delle caselle
		    di rest *)        
	try from_node visited a 
        with NotFound -> from_list (a::visited) rest
                  (* il nodo a e' stato visitato *)
  in from_node [] start

(* implementazione alternativa, senza mutua ricorsione: *)
(* gpath:  'a graph -> 'a -> ('a -> bool) -> 'a list *)
let gpath g start p =
  let rec aux visited = function
      [] -> raise NotFound
    | x::rest -> if List.mem x visited 
                 then aux visited rest
                 else if p x then [x]
                      else try x:: aux (x::visited) (vicini x g)
                           (* ricerca a partire dai vicini di x *)
                           with NotFound ->
                                 aux (x::visited) rest
                         (* ricerca a partire dai "fratelli" di x *)
  in aux [] [start]

(* ATTENZIONE: a differenza che negli algoritmi di visita in cui non 
   si riporta un cammino, qui 
   non si mescolano i vicini di un nodo con i suoi fratelli *)

(** ------------------------------------------------ **)

(** Problema: attraversamento di un labirinto, da una casella
   di entrata a una casella di uscita, senza passare per caselle
   che contengono un mostro, e raccogliendo gli oggetti che
   si trovano nelle altre caselle (se ce ne sono). 

   Si deve riportare un cammino (quindi ricerca di un cammino mediante
   backtracking), ma non si puo' passare per caselle con il mostro. E
   si devono raccogliere gli oggetti. *)

(** un labirinto e' un grafo i cui nodi hanno dei contenuti **)
(* Contenuti possibili: mostro o oggetti *)
type content = Mostro | Obj of string

(* Contenuto delle caselle:
   Lista associativa, in cui si associa un valore solo alle
   caselle che contengono qualcosa *)
type 'a contents = ('a * content list) list 

(* esempio *)
let contents =
  [(1,[Obj "oro"]); (2,[Mostro]); (4,[Obj "computer";Obj "penna"]);
   (7,[Mostro; Obj "libro"])]

(* un labirinto e' un: 'a graph * 'a contents *)
type 'a labirinto = 'a graph * 'a contents

(* esempio *)
let labirinto = (grafo,contents)

(* funzioni di supporto *)
(* trovare il contenuto di una casella *)
(* content: 'a -> ('a * 'b list) list -> 'b list *)
let content x contenuti =
  try List.assoc x contenuti
  with Not_found -> []

(* verificare se una casella contiene un mostro:
   has_monster:  'a -> ('a * content list) list -> bool *)
let has_monster x contenuti =
 List.mem Mostro (content x contenuti)

(* primo passo: adattiamo l'algoritmo di ricerca di cammino tenendo
   in considerazione la presenza dei mostri, ma ignorando la raccolta 
   degli oggetti *)
(* path: 'a labirinto -> 'a -> 'a -> 'a list *)
let path ((grafo,contenuti): 'a labirinto) ingresso uscita =
  (* cerca_da: 'a list -> 'a  -> 'a list   Ricerca da una singola casella *)
  let rec cerca_da visited casella =
    if List.mem casella visited then raise NotFound
    else 
    (* siamo arrivati? *)
      if casella = uscita 
      then [casella]
      else (* la casella contiene un mostro? *)
	if has_monster casella contenuti
	then raise NotFound
	else (* passiamo per casella e proseguiamo con 
		una delle caselle accessibili *)
	  casella :: cerca_da_una_tra (casella::visited) (vicini casella grafo)
  (* cerca_da_una_tra: 'a list -> 'a list *)
  and cerca_da_una_tra visited = function
      [] -> raise NotFound
    | x::rest -> 
	try cerca_da visited x 
	with NotFound -> cerca_da_una_tra (x::visited) rest
  in cerca_da [] ingresso

(* Ora vogliamo in uscita il cammino e la lista degli oggetti raccolti*)

(* path:  'a labirinto -> 'a -> 'a -> 'a list * content list *)
let path ((grafo,contenuti): 'a labirinto) ingresso uscita =
  (* cerca_da: 'a list -> 'a  -> 'a list   Ricerca da una singola casella *)
  let rec cerca_da visited casella =
    if List.mem casella visited then raise NotFound
    else 
    (* siamo arrivati? *)
      if casella = uscita 
      then ([casella],content casella contenuti)
      else (* la casella contiene un mostro? *)
	if has_monster casella contenuti
	then raise NotFound
	else 
	  let (cammino,oggetti) =
	    cerca_da_una_tra (casella::visited) (vicini casella grafo)
	  in (casella::cammino,(content casella contenuti) @ oggetti)
  (* cerca_da_una_tra: 'a list -> 'a list -> 'a list *)
  and cerca_da_una_tra visited = function
      [] -> raise NotFound
    | x::rest -> 
	try cerca_da visited x 
	with NotFound -> cerca_da_una_tra (x::visited) rest
  in cerca_da [] ingresso

(****************************************)
(* Dall'esame di febbraio 2012 *)

(* A seconda del problema, le funzioni ausiliarie possono avere
   parametri addizionali *)

type shops = (int * string list) list
type city = (int * int) list * shops

(*  vicini : 'a -> ('a * 'a) list -> 'a list *)
(* il grafo non e' orientato *)
let rec vicini x = function
    [] -> []
  | (a,b)::rest ->
      if a=x then b::vicini x rest
      else if b=x then a::vicini x rest
      else vicini x rest

exception Fail

(* funzione ausiliaria *)
(* diff : 'a list -> 'a list -> 'a list *)
(* diff lst1 lst2 = lista che si ottiene da lst2 togliendo tutte
                    le occorrenze di tutti gli elementi di lst1 *)
let diff lst1 lst2 = 
  List.filter
    (function x -> not (List.mem x lst1)) lst2

(* compra : string list -> city -> int -> int list *)
let compra lista (graph,shops) start =
  let rec fromnode visited lista nodo =
    (* lista contiene gli oggetti che ancora si devono comprare *)
    if List.mem nodo visited then raise Fail
    else 
      let inshop = (* oggetti che si possono comprare in nodo *)
	try List.assoc nodo shops
	with Not_found -> [] in
      if List.for_all 
	  (function x -> List.mem x inshop) lista
         (* tutti gli oggetti da comprare sono presenti in nodo *)
      then [nodo]
      else nodo::fromlist (nodo::visited) 
		   (diff inshop lista) (* togliamo da lista gli oggetti
                                          che possiamo comprare in nodo *)
                (* o direttamente:
		   (List.filter  
		      (function x -> not (List.mem x inshop)) lista) *)
		   (vicini nodo graph)
  and fromlist visited lista = function
      [] -> raise Fail
    | n::rest ->
	try fromnode visited lista n
	with Fail -> fromlist (n::visited) lista rest
  in fromnode [] lista start

let grafo = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);
             (3,5);(4,6);(4,7);(5,7);(6,7)]
        (* 
                  1
                / | \
               3--2--4
               |   \/ \
               5---7---6
         *)
let shops =
  [(2,["carta";"penna"]); (4,["latte";"uova";"pane"]); 
   (5,["biglietto bus";"tabacco"]); (6,["trapano";"chiodi"])]

let tobuy = ["chiodi";"tabacco";"uova";"penna";"pane"]

(*
# compra tobuy (grafo,shops) 1;;
- : int list = [1; 2; 3; 5; 7; 4; 6]
*)