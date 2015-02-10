(* List.sort: 
val sort : ('a -> 'a -> int) -> 'a list -> 'a list
      Sort a list in increasing order according to a comparison
      function. The comparison function must return 0 if it arguments
      compare as equal, a positive integer if the first is greater,
      and a negative integer if the first is smaller (see Array.sort
      for a complete specification). For example, Pervasives.compare
      is a suitable comparison function.
*)

(* Esempio d'uso di List.sort *)
(* cmp : 'a * 'b -> 'c * 'b -> int *)
(* cmp (_,x) (_,y) = 1 (prima coppia "maggiore") se x e' minore
                     di y *)
let cmp (_,x) (_,y) = 
  - (compare x y)

(* pairlistsort : ('a * 'b) list -> ('a * 'b) list *)
(* ordina secondo valori decrescenti del secondo elemento delle
   coppie *)
let pairlistsort lst = 
  List.sort cmp lst

let lista = [(3,4); (6,1); (7,20)]

(* tipo di List.sort : ('a -> 'a -> int) -> 'a list -> 'a list 
       funzione di ordine superiore *)

(* http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html *)
(* Iterators, list scanning, list searching .... *)

(* FUNZIONI DI ORDINE SUPERIORE *)
(* sommatorie:
      sum : (int -> int) -> int -> int -> int 
      sum f n m = somme di f(k) per k=n...m *)
let rec sum f n m =
  if n>m then 0
  else f n + sum f (n+1) m;;

(* square : int -> int *)
let square x = x*x
(* sumsquare : int -> int -> int *)
let sumsquare = sum square

(* ------------- map ------------ *)
(* prendi tutti i primi elementi di una lista di coppie *)
(* primi : ('a * 'b) list -> 'a list *)
let rec primi = function
    [] -> []
  | (x,_)::rest -> x::primi rest

(* intesta_a_tutti: *)
(* intesta_a_tutti : 'a -> 'a list list -> 'a list list *)
let rec intesta_a_tutti x = function
    [] -> []
  | lst::rest -> (x::lst)::intesta_a_tutti x rest

(* tutti_in_testa: 'a list -> 'a list -> 'a list list *)
(* tutti_in_testa lst [x1;...;xn] = [x1::lst;...;xn::lst] *)
let rec tutti_in_testa lst = function
    [] -> []
  | x::rest -> (x::lst)::tutti_in_testa lst rest

(* sono esempi di una funzione generale:
   map : ('a -> 'b) -> 'a list -> 'b list
   map f [x1;...;xn] = [f x1;....;f xn] *)
let rec map f = function
    [] -> []
  | x::rest -> f x :: map f rest;;

(* primi = map fst
   intesta_a_tutti x = map (function lst -> x::lst) 
   tutti_in_testa lst = map (function x -> x::lst)
*)

(* tipo di map:
   map f lst e` corretta se gli elementi di lst sono
                nel dominio di f *)

(* esempi di applicazione di map *)
(* inits: 'a list -> 'a list list
inits lst = lista con tutti i segmenti iniziali di lst
        inits [1;2;3;4] = [[1];[1;2];[1;2;3];[1;2;3;4]]
pdf (11-ordsupliste p 4) *)

(* cons : 'a -> 'a list -> 'a list *)
let cons x rest = x::rest

let rec inits = function
    [] -> []
  | x::rest -> [x] :: List.map (cons x) (inits rest)

(* attenzione: [x]::.... e non x::... *)

(* il codice morse *)
(* definizione di un nuovo tipo, enumerato *)
type segnale = Linea | Punto 
               | Pausa | Errore (* per la codifica di caratteri 
                                   di cui non e` dato il codice *)
let morse =
  [ 'A', [Punto;Linea] ; (* non servono le parentesi per le coppie *)
    'B', [Linea;Punto;Punto;Punto];
    'C', [Linea;Punto;Linea;Punto];
    'D', [Linea;Punto;Punto];
    'E', [Punto] ;
    'F', [Punto;Punto;Linea;Punto];
    'G', [Linea;Linea;Punto];
    'H', [Punto;Punto;Punto;Punto];
    'I', [Punto;Punto];
    'J', [Punto;Linea;Linea;Linea];
    'K', [Linea;Punto;Linea];
    'L', [Punto;Linea;Punto;Punto];
    'M', [Linea;Linea];
    'N', [Linea;Punto];
    'O', [Linea;Linea;Linea];
    'P', [Punto;Linea;Linea;Punto];
    'Q', [Linea;Linea;Punto;Linea];
    'R', [Punto;Linea;Punto];
    'S', [Punto;Punto;Punto];
    'T', [Linea];
    'U', [Punto;Punto;Linea];
    'V', [Punto;Punto;Punto;Linea];
    'W', [Punto;Linea;Linea];
    'X', [Linea;Punto;Punto;Linea];
    'Y', [Linea;Punto;Linea;Linea];
    'Z', [Linea;Linea;Punto;Punto]
  ]
    
(* codifica di una sequenza di caratteri (alfabetici maiuscoli) +
   spazio.   Lo spazio e` codificato con [Pausa] *)
(* encode: char -> segnale list *)
(* utilizza la variabile globale morse *)
let encode = function
    ' ' -> [Pausa]
  | c -> 
      try List.assoc c morse
      with Not_found -> [Errore] 

(* encode: char list -> segnale list list *)
let  encode_msg messaggio = 
  List.map encode messaggio

(* explode: string -> char list *)
let explode s =
  let rec aux n result =
    if n < 0 then result
    else aux (n-1) (s.[n] :: result) in
  aux (String.length s - 1) []

let msg = encode_msg (explode "CIAO PIPPO") 
let msg2 = encode_msg (explode "CiAO PIPPO")


(* -------------- iter ------------- *)
(* iter : ('a -> unit) -> 'a list -> unit
List.iter f [a1; ...; an] applies function f in turn to a1; ...; an.
*)
let rec iter f = function
    [] -> ()
  | x::rest -> f x; iter f rest

(* iter (function x -> print_endline (string_of_int x)) [1;2;3;4] *)

(* --------- composizione di funzioni ------------ *)
let (@@) f g x = f(g x)
(* iter (print_endline @@ string_of_int) [1;2;3;4] *)

(* -------------- filter ------------- *)
(* costruire una lista contentente soltanto gli interi
   maggiori di 100 contenuti in una lista data *)
(*  maggiori_di_cento: int list -> int list *)
let rec maggiori_di_cento = function
    [] -> []
  | x::rest ->
       if x>100 then x::maggiori_di_cento rest
       else maggiori_di_cento rest

(* funzionale filter per il filtraggio di una lista *)
(* filter : ('a -> bool) -> 'a list -> 'a list *)
let rec filter p =  function
    [] -> []
  | x::rest ->  if p x then x::filter p rest
                else filter p rest;;

(* greaterthan : 'a -> 'a -> bool *)
let greaterthan x y = y>x
(* greaterthan 100 : int -> bool
         proprieta' di essere maggiore di 100 *)

let maggiori_di_cento = filter (greaterthan 100)

(* "filtraggio" di un insieme di caselle rispetto alla 
   proprieta` di essere interne al labirinto *)
(*  in_labirinto : int -> int * int -> bool *)
let in_labirinto dim (r,c) =
  r >= 0 && c >= 0 && r < dim && c < dim
(* "filtrare" una lista di caselle conservando solo
   quelle interne al labirinto *)
(* filter_vicini: int -> (int * int) list -> (int * int) list *)
let rec filter_vicini dim = function
    [] -> []
  | casella::rest ->
      if in_labirinto dim casella
      then casella::filter_vicini dim rest
      else filter_vicini dim rest

let filter_vicini dim = filter (in_labirinto dim)
let filter_vicini = filter @@ in_labirinto 

(* tipi:
   filter: ('a -> bool) -> 'a list -> 'a list
   (@@): ('a1 -> 'b1) -> ('c1 -> 'a1) -> 'c1 -> 'b1
      filter puo' essere il primo argomento di @@ con
         'a1 = 'a -> bool
         'b1 = 'a list -> 'a list

   (@@) filter: ('c1 -> 'a -> bool) -> 'c1 -> 'a list -> 'a list
   in_labirinto: int -> int * int -> bool
          in_labirinto puo' essere argomento di
          (@@) filter, con 'c1 = int
                           'a = int * int

   (@@) filter in_labirinto: 
            int -> (int * int) list -> (int * int) list

  Il primo argomento di filter deve essere un predicato che
  si puo' applicare agli elementi della lista secondo argomento
*)

(* ---------- for_all ------------ *)
(* verificare se tutti gli interi di una lista sono maggiori di 100 *)
(* tutti_maggiori_di_100 : int list -> bool *)
let rec tutti_maggiori_di_100 = function
    [] -> true (* perche' true? *)
  | x::rest ->
         greaterthan 100 x && tutti_maggiori_di_100 rest

(* funzionale for_all *)
(* for_all : ('a -> bool) -> 'a list -> bool *)
let rec for_all p = function
    [] -> true
  | x::rest -> p x & for_all p rest

let tutti_maggiori_di_100 = for_all (greaterthan 100)

(* definizione dell'opposto di mem
   nonmem x lst = x non appartiene a lst *)
let rec nonmem x = function
    [] -> true
  | y::rest -> x <> y && nonmem x rest

(* nonmem x = for_all (function y -> x <> y)
            = for_all (function y -> (<>) x y)
            = for_all ((<>) x)                       *)
let nonmem x = for_all ((<>) x) 

(* verificare se due insiemi sono disgiunti:
    disjoint : 'a list -> 'a list -> bool
   tutti gli elementi di un insieme devono essere diversi da tutti
   gli elementi dell'altro *)
(* x diverso da tutti gli elementi di lst =
   not (List.mem x lst) *)
let rec disjoint set = function
    [] -> true
  | x::rest -> 
      not (List.mem x set) 
	&& disjoint set rest
(* disjoint set lst = tutti gli elementi di lst hanno la proprieta`
                      di non appartenere a set *)

let disjoint set1 set2 =
  List.for_all 
    (function x -> not (List.mem x set1)) set2

(* si puo' usare la composizione di funzioni per rappresentare
   la funzione:  function x -> not (List.mem x set1)  ? *)
(* swap_args : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c *)
let swap_args f x y = f y x 
(* member : 'a list -> 'a -> bool *)
let member = swap_args List.mem

let disjoint set1 set2 =
  List.for_all (not @@ (member set1)) set2
(* = 
   List.for_all (not @@ ((swap_args List.mem) set1)) set2 *)

(* verificare se in una lista associativa ogni chiave e` associata
   ad un unico valore: se contiene (x,y) e (x,z) allora x=z.
   (Esame di settembre 2011) *)
(* functional : ('a * 'b) list -> bool *)
let rec functional = function
    [] -> true
  | (x,y)::rest ->
      List.for_all (function (z,w) -> z<>x or w=y) rest
	&& functional rest

(* --------- exists ---------- *)
(* mem x lst = esiste un elemento di lst uguale a x *)
  (*   mem: 'a -> 'a list -> bool *)
let rec mem x = function
    [] -> false
  | y::rest ->
      x=y or mem x rest

(* exists : ('a -> bool) -> 'a list -> bool *)
let rec exists p = function
    [] -> false
  | x::rest -> p x or exists p rest

(* mem x = exists (function y -> x=y)
         = exists ((=) x)                   *)

(* exists p lst = not (for_all p lst)
   exists p     = not @@ (for_all p)
   for_all p    = not @@ (exists p)         *)

(* --- applicazioni: insiemi finiti --- *)
(* 11-ordsupliste pag. 16-21 *)

(* powerset : 'a list -> 'a list list *)
let rec powerset = function
    [] -> [[]]
  | x::rest -> let powerset_rest = powerset rest
               in powerset_rest @ List.map (cons x) powerset_rest;;

(* cartprod : 'a list -> 'b list -> ('a * 'b) list *)
let rec cartprod set1 set2 = 
  match set1 with
    [] -> []
  | x::rest ->  
      (List.map (function y -> (x,y)) set2) @ cartprod rest set2
(* oppure, scandendo il secondo insieme *)
let rec cartprod set = function
    [] -> []
  | x::rest ->  
      (List.map (function y -> (y,x)) set) @ cartprod set rest 