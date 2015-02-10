(* 01-intro *)
(*
PROGRAMMAZIONE FUNZIONALE

http://cialdea.dia.uniroma3.it/teaching/pf/

  - alto livello: un programma e` una funzione
  - programmazione dichiarativa

Esempio:

  gcd(n,m) = n           se n=m
  gcd(n,m) = gcd(n-m,m)  se n>m
  gcd(n,m) = gcd(n,m-n)  se n<m
*)

(* gcd: int * int -> int *)
let rec gcd(m,n) = 
  if n=m then n 
  else 
    if n>m then gcd(n-m,m)
    else gcd(n,m-n)

(* INFERENZA DEI TIPI *)

(* 
modalita` interattiva:

# gcd(28,16);;
- : int = 4

*)

(* fattoriale:

n! = 1 * 2 * ... * n-1 * n 
   = (n-1)! * n
*)

(* fact: int -> int *)
let rec fact n =
  if n=0 then 1
  else n * fact(n-1)

(* INFERENZA DEI TIPI E POLIMORFISMO *)

(* first : 'a * 'b -> 'a *)
let first (x,y) = x

(* id : 'a -> 'a *)
let id x = x

(* LE FUNZIONI SONO OGGETTI DI PRIMA CLASSE *)

(*  comp : ('a -> 'b) * ('c -> 'a) -> 'c -> 'b *)
let comp (f,g) =
  function x -> f(g x)

(* Esempio di applicazione *)
(* succ: int -> int e` una funzione predefinita, riporta il successore
   di un intero. Definiamo anche: *)
(* double: int -> int, che applicata a un intero ne riporta il doppio *)
let double n = 2 * n
(*
# comp (double,succ);;
- : int -> int = <fun>

comp(double,succ) e' una funzione da interi a interi: applicata a un 
intero n, riporta il doppio del suo successore:

# comp (double,succ) 7;;
- : int = 16
*)

(* ccomp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let ccomp f =
  function g ->
    function x -> f(g x)

(* modalita` interattiva *)
(* ciclo: lettura-valutazione-stampa *)
(* Espressioni, tipi, valori, dichiarazioni *)
(* ogni ESPRESSIONE ha un TIPO e un VALORE *)

(* AMBIENTE di valutazione
   funzioni primitive, parole chiave
   Ambiente del "modulo" Pervasives
      Tipi predefiniti:
           http://caml.inria.fr/pub/docs/manual-ocaml/manual033.html#toc148
      Eccezioni
      Valori: 
           http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html

02-ocaml, pag. 11-13 --  (1-3)
*)

(* DICHIARAZIONI *)
(* let <variabile> = <espressione> *)

let tre = 3
let quattro = tre + 1

(* square : int -> int *)
let square =
  function n -> n* n

(* float_square : float -> float *)
let float_square =
  function n -> n *. n

(*
  let <nome-funzione> = 
           function <parametro-formale> -> <espressione>
  let <nome-funzione> <parametro-formale> = <espressione>
*)
let double n = 2 * n
let treble n = 3 * n

let sixtimes = comp(double,treble)

(*
let comp (f,g) =
  function x -> f(g x)

let comp (f,g) x = f(g x)
*)

(* Uso delle parentesi:
    double (square 3)
    double square 3        =>  (double square) 3
*)

(* 
let ccomp f =
  function g ->
    function x -> f(g x)


   ccomp double square 3    => ((ccomp double) square) 3
*)

(* gestione dell'ambiente come stack *)
let due = 2
let due = "due"

(* scopo statico *)
(* 02-ocaml, pag 14-15 (4-6) *)
let sei = 6
let persei n = sei * n
let sei = "sei"
let sei = 7

(* dal punto di vista pratico:
definizione di f,
poi della funzione g che chiama f,
test,
correzione di f,
g ancora non funziona! *)

(* gestione dell'ambiente nella chiamata di funzioni *)
(* 02-ocaml, pag 16 (7) *)

(* TIPI: tipizzazione forte *)
(* tipi semplici predefiniti: 
             int, float, bool, string,char, unit
   Niente overloading
   operatori booleani,
   operatori di confronto,
   tipi con uguaglianza *)

(* ESPRESSIONI CONDIZIONALI *)
(* sort: 'a * 'a -> 'a * 'a *)
let sort (x,y) =
   if x>y then (y,x)
   else (x,y)
(* perche' sort ha il tipo sopra specificato? *)

let qualcuno =
   4 + (if square 3 > 10 
        then double tre
        else treble sei)

(* OGNI ESPRESSIONE HA UN VALORE E UN TIPO CHE SI PUO` DETERMINARE
   A TEMPO DI COMPILAZIONE *)

let errore =
  if sei > due 
  then "sei maggiore di due"
  else 0

(* if E then F else G
           che tipo ha?
           quali tipi devono avere E, F e G?
           quale valore ha?
   Regola di valutazione
   02-ocaml, p.22 (7)
*)

(* operatori booleani. 02-ocaml, p.23-24 (8-9) *)
(*   se E e F sono bool:
          not E:  bool
          E && F: bool 
          E || F: bool  (anche: E or F) 
     Regole di valutazione
*)


(* TIPI: coppie e tuple *)
let a = (7,"pippo")
let b = (3>4, 5.1, "antonio")
let c = (3>4, (5.1, "antonio"))
let d = ((3>4, 5.1), "antonio")
let e = (double,6,'p')

(* qual e` il tipo di e?
   LE FUNZIONI SONO OGGETTI DI PRIMA CLASSE *)

(* costruttori e selettori di tipo, fst e snd *)
(* polimorfismo, istanze di un tipo polimorfo
            fst a
            fst (double,6)
*)

(* selettori per le triple? *)

(* quanti argomenti ha fst? *)

let quorem (n,m) =
  (n/m, n mod m)

let (a,b) = quorem(20,7)

(* FUNZIONI DI ORDINE SUPERIORE *)
(* 01-intro p.29 (10) *)
(*  comp : (('a -> 'b) * ('c -> 'a)) -> ('c -> 'b) *)
let comp (f,g) x = f(g x)

(* ccomp : ('a -> 'b) -> (('c -> 'a) -> ('c -> 'b)) *)
let ccomp f g x = f(g x)

(* NB: inferenza dei tipi *)