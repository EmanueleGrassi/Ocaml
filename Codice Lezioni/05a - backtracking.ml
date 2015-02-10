(** Uso delle liste per rappresentare tipi astratti di dati *)

(** Tipo astratto dizionario e liste associative:
           ('a * 'b) list *)
(* ricerca: 
   assoc: 'a -> ('a * 'b) list -> 'b *)
exception NotFound
let rec assoc k = function
    [] -> raise NotFound
  | (k1,v)::rest -> if k = k1 then v   
                     else assoc k rest
(* ma d'ora in poi usiamo List.assoc, che solleva Not_found *)

(* inserimento:
   inserisci : 'a * 'b -> ('a * 'b) list -> ('a * 'b) list *)
let rec inserisci (k,v) assoc =
      (k,v)::assoc

(* cancellazione di una chiave: per esercizio *)

(** Insiemi finiti *)
(* operazioni: test di appartenenza, unione, intersezione, .... *)
(*    mem: 'a -> 'a list -> bool
      mem x lst = true se lst contiene x *)
let rec mem x = function
  [] -> false
  | y::rest ->
       x=y or mem x rest

(* ma d'ora in poi usiamo List.mem *)

(* Esempio: rappresentazione di una "matrice", dove alcune caselle
   possono contenere degli oggetti (rappresentati da stringhe):
   lista associativa dove le chiavi sono le stringhe e i valori sono 
   liste di caselle (quelle che contengono l'oggetto corrispondente
   alla stringa). Ogni casella e' una coppia (riga,colonna) *)
let contents =
  [("oro",[(1,0);(3,1);(4,3)]);
   ("argento",[(0,1);(2,4)]);
   ("mostro",[(0,2);(1,1);(1,3);(2,3);(3,0);(4,2)])]

(* per verificare se una casella contiene un determinato oggetto: *)
(*  has : 'a -> 'b -> ('a * 'b list) list -> bool *)
(* has x casella contenuti = true se casella ha il contenuto x
	secondo la lista associativa contenuti *)
let has x casella contenuti =
  try
    List.mem casella (List.assoc x contenuti)
  with Not_found -> false
(* N.B: le eccezioni si propagano *)

(* per "raccogliere" tutti gli oggetti contenuti in una casella *)
(*  find_content : ('a * 'b list) list -> 'b -> 'a list *)
let rec find_content contenuti c =
  match contenuti with
    [] -> []
  | (x,caselle)::rest ->
      if List.mem c caselle then x::find_content rest c
      else find_content rest c

(* se vogliamo raccogliere tutti i contenuti di un insieme (lista)
   di caselle: *)
(* raccolti : ('a * 'b list) list -> 'b list -> 'a list *)
let rec raccolti contenuti = function
    [] -> []
  | casella::rest ->
      (find_content contenuti casella) @ (raccolti contenuti rest)

(**** BACKTRACKING ****)
(*
Tecnica del backtracking:
Problemi che riguardano la ricerca di un insieme di soluzioni (o di
una soluzione ottima) che soddisfino condizioni date.

Soluzione costruibile incrementalmente.

Backtracking: costruire la soluzione aggiungendo un elemento alla volta
e usare un criterio per controllare se la sequenza parziale ha
possibilita` di successo.
*)

(* PROBLEMA: Dato un insieme S di numeri interi positivi e un intero
N, determinare un sottoinsieme Y di S tale che la somma degli elementi
di Y sia uguale a N.

Esempio: S={8,5,1,4}, N=9.

Aggiungiamo una componente alla volta.

Spazio di ricerca delle soluzioni: albero etichettato da sottoinsiemi di S.

Foglie: situazioni non ulterirmente espandibili; alcune di esse
possono rappresentare una soluzione (successo) altre no (fallimento)

        _____________________ vuoto _____________________
        |               |               |               |
  ____ {8} ___       __{5}___           {1}             {4}
  |     |     |      |      |            |            FAIL
{8,5} {8,1} {8,4}   {5,1} {5,4}        {1,4}            
FAIL SUCCESS FAIL   FAIL  SUCCESS     FAIL 

Fallimento su un nodo Y in due casi:
- la somma degli elementi di Y e' maggiore di N
- la somma degli elementi di Y e' minore di N e non ci sono altri elementi
  da aggiungere

Quando "siamo" in un nodo (un determinato stadio della ricerca) abbiamo:
- la "soluzione parziale" Y (inizialmente vuota)
- un insieme X di elementi che si possono ancora aggiungere alla soluzione
  (inizialmente X=S)
- il totale da raggiungere (N)

Criterio per scartare una soluzione parziale: la somma dei suoi elementi e' 
maggiore di N. 

E' necessario utilizzare una funzione ausiliaria (con argomenti supplementari)
*)

(* utility: somma degli elementi di una lista *)
let rec sumof = function
    [] -> 0
  | x::rest -> x+sumof rest

(* eccezione per segnalare il fallimento *)
exception NotFound


(* subset_search : int list -> int -> int list *)
let subset_search set n =
  (* aux : int list ->  int list -> int list *)
  (* funzione ausiliaria che implementa il ciclo *)
  (* i casi sono distinti a seconda della forma di altri (X) *)
  let rec aux solution altri = 
    let somma = sumof solution in 
    (* caso di fallimento *)
    if somma > n then raise NotFound
    else match altri with
      [] -> (* non si possono aggiungere altri elementi *)
        if somma = n then solution
	else raise NotFound
    | x::rest -> 
	(* proviamo ad aggiungere x, se troviamo una soluzione, bene,
	   altrimenti proviamo senza x *)
        try aux (x::solution) rest 
        with NotFound -> aux solution rest 
  in aux [] set

(* 
# subset_search [8;5;1;4] 9;;
- : int list = [1; 8]
*)

(* possiamo evitare di calcolare la somma degli elementi della soluzione,
   se aggiungiamo a aux un parametro intero che rappresenta il valore che 
   manca per raggiungere il totale desiderato. 
   Inizialmente e' uguale a N.
   Quando e' negativo abbiamo sballato *)

(* subset_search : int list -> int -> int list *)
let subset_search set n =
  (* aux : int list -> int -> int list -> int list *)
  let rec aux solution tot altri = 
    if tot < 0 then raise NotFound
    else match altri with
      [] -> (* non si possono aggiungere altri elementi *)
        if  tot = 0 then solution
	else raise NotFound
    | x::rest -> 
	(* proviamo ad aggiungere x, se troviamo una soluzione, bene,
	   altrimenti proviamo senza x *)
        try aux (x::solution) (tot - x) rest 
        with NotFound -> aux solution tot rest 
  in aux [] n set


(* ricerca di tutte le soluzioni *)
(* non si sollevano mai eccezioni, si riporta sempre una lista - eventualmente
   vuota *)

(* search_all : int list -> int -> int list list *)
let search_all set n =
  (* aux : int list -> int -> int list -> int list list *)
  let rec aux solution tot altri = 
    if tot < 0 then [] (* non ci sono soluzioni *)
    else match altri with
      [] -> (* non si possono aggiungere altri elementi *)
        if  tot = 0 then [solution]
	    (* lista con l'unica soluzione trovata *)
        else [] (* non ci sono soluzioni *)
    | x::rest -> 
	(* concateniamo (con @) le soluzioni che troviamo aggiungendo x
           con quelle che troviamo senza x *)
        (aux (x::solution) (tot - x) rest)
        @ (aux solution tot rest) 
  in aux [] n set

(*
# search_all [8;5;1;4] 9;;
- : int list list = [[1; 8]; [4; 5]]
*)

(** Vedere sul libro il problema delle n regine **)