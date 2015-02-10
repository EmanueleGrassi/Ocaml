(* algoritmi iterativi mediante ricorsione
   dichiarazioni locali
   eccezioni
   pattern matching  *)

(* -------------------------------- *)
(* definizione ricorsiva di funzioni *)
(* fattoriale di n = n * (n-1) * .... * 1 
                   = n * (fattoriale di n-1) *)
(* fact: int -> int *)
let fact_errore n =
  if n=0 then 1
  else n * fact_errore (n-1)

let rec fact n =
  if n=0 then 1
  else n * fact (n-1)

(* cosa succede se definiamo, in quest'ordine, due funzioni f? *)
let f n = n*2

let f n =
  if n=0 then 1
  else n * f (n-1)

(* let e rec sono Parole Chiave *)

(* 04-ricorsione-1, pag. 5-6 (11-12) *)
(* ------------------------------- *)

(* obiettivo: scrivere un programma che, data una stringa 
   che rappresenta un'operazione aritmetica semplice 
   tra interi positivi, ne riporti il valore numerico.

   Ad esempio il valore riportato per la stringa "34+12" e` l'intero
   46.  Le operazioni consentite sono somma, differenza, prodotto, 
   divisione (intera) *)

(** Progetto:
Funzione principale
evaluate: string -> int
   evaluate s = valore numerico dell'espressione rappresentata dalla stringa
                s. Errore se s non rappresenta un'espressione aritmetica 
                semplice come descritto sopra.
Sottoproblemi utili:
split_string : string -> int * char * int
   split_string s = (n,op,m) dove n e' il primo operando,
                    op e' il carattere che rappresenta l'operazione,
                    m e' il secondo operando.
Per risolvere il problema split_string:
- cercare il primo carattere non numerico:
    primo_non_numerico: string -> int
         primo_non_numerico s = indice del primo carattere non numerico
                                nella stringa s. Errore se non c'e`.
- estrarre una parte di una stringa:
    substring : string -> int -> int -> string
         substring s j k = sottostringa di s che va dalla posizione j 
                           alla posizione k.
Funzioni predefinite utili:
- String.sub : string -> int -> int -> string
     String.sub s start len returns a fresh string of length len,
    containing the substring of s that starts at position start and has
    length len.
- int_of_string: : string -> int
    Convert the given string to an integer. 
**)


(* Il modulo String della libreria standard di OCaml:
   http://caml.inria.fr/pub/docs/manual-ocaml/libref/String.html *)

(* sottoproblema 1: data una stringa, riportare la posizione del 
   primo carattere non numerico che occorre in essa (quello che
   rappresentera` l'operazione) *)
(* sotto-sottoproblema 1a: determinare se un carattere e` numerico *)

(* numeric : char -> bool *)
let numeric c =
  c >= '0' && c <= '9'

(* algoritmo: iniziando con i=0, si incrementa finche' il carattere
   in posizione i e' numerico *)
(* sotto-sottoproblema 1b:
   data una stringa e un indice i, riportare la posizione del primo
   carattere non numerico della stringa a partire dalla posizione i *)

(* subproblem: string * int -> int *)
let rec subproblem(str,i) =
  if not (numeric str.[i]) then i
  else subproblem(str,i+1)
(* Implementa un'iterazione *)

(* cosa succede valutando subproblem("pippo",100) ? *)

(* soluzione del sottoproblema 1 = soluzione del sottoproblema 1b
   con i=0.  Cio\`e inizializzazione dell'iterazione *)
(* primo_non_numerico: string -> int *)
let primo_non_numerico str =
  subproblem(str,0)

(* cosa succede valutando  primo_non_numerico "127" ? *)

(* dichiarazioni locali: la funzione subproblem serve soltanto a
   primo_non_numerico *)
let  primo_non_numerico str =
  let rec subproblem(str,i) =
    if not (numeric str.[i]) then i
    else subproblem(str,i+1)
  in subproblem(str,0)

(* il parametro str e` inutile *)
let primo_non_numerico str =
  (* aux: int -> int *)
  let rec aux i =
    if not (numeric str.[i]) then i
    else aux (i+1)
  in aux 0

(* ------------------------------------ *)
(* <dichiarazione-let> in <espressione>
   e` un'espressione.
   Qual e` il suo tipo?
   Qual e` il suo valore?
   Come viene valutata?
*)

(* Dichiarazioni locali: esempio.
   dati tre interi n, m e k,
   determinare il quoziente e il resto di n+m diviso k *)
(* esempio: int * int * int -> int * int *)
let esempio (n,m,k) =
  ( (n+m)/k, (n+m) mod k)

(* n+m viene calcolato 2 volte *)

let esempio(n,m,k) =
  let somma = n+m 
  in (somma/k, somma mod k)

(* equivalentemente: *)
let esempio(n,m,k) =
  (function somma -> (somma/k, somma mod k)) (n+m)

(* let x=E in F
   equivale a
   (function x -> f) E
*)

(* esempio: ridurre ai minimi termini una frazione rappresentata
   mediante una coppia di interi *)
(* gcd : int * int -> int *)
let rec gcd (m,n) = 
  if n=m then n 
  else if n>m then gcd(n-m,m)
       else gcd(n,m-n)

(* fraction : int * int -> int * int *)
let fraction (n,d) =
    let com = gcd(n,d)
    in  (n/com, d/com)
(* ha senso rendere gcd locale a fraction? *)

(* 03-dichiarazioniLocali, p. 4-5, 8-10 (13-17)*)
(* ------------------------------------ *)

(* sottoproblema 2: data una stringa rappresentante un'espressione
   aritmetica semplice, riportare il primo operando, l'operazione
   (come carattere), e il secondo operando *)
(* split_string: string -> int * char * int *)

(* nel modulo String:
val sub : string -> int -> int -> string

String.sub s start len returns a fresh string of length len,
containing the substring of s that starts at position start and has
length len.
*)
 
(* --------------------------------- *)
(* FUNZIONI IN FORMA CURRIFICATA *)
(* val sub : string -> int -> int -> string *)

(* gcd : int * int -> int *)
let rec gcd (m,n) = 
  if n=m then n 
  else if n>m then gcd(n-m,m)
       else gcd(n,m-n)
(* applicazione: gcd (24,16) *)

(* gcd2 : int -> int -> int *)
let rec gcd2 m n =
  if n=m then n 
  else if n>m then gcd2 (n-m) m
       else gcd2 n (m-n)
(* applicazione: gcd2 24 16 *)
(*               (gcd2 24) 16 *)

(* ccomp : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b *)
let ccomp f =
  function g ->
    function x -> f(g x)

let ccomp f g x = f (g x)

(* String.sub:
   String.sub "pippo&pluto" 3 5 
*)

(* applicazione parziale di funzioni *)
(* plus/times: int -> (int -> int) *)
let plus n m = n+m
(* plus = function n -> (function m -> n+m) *)
let times n m = n*m
(* times = function n -> (function m -> n*m) *)
let f = ccomp (times 3) (plus 100)

(* 08-ordsup p. 9 e 11-12 (max, operatori infissi) (18-21) *)
(* ------------------------- *)

(*
Se i e' la posizione del primo carattere non numerico di str,
il primo operando occupa le posizioni da 0 a i-1,
l'operazione il carattere i,
il secondo operando le posizioni da i+1 all'ultima.
*)

(* sottoproblema: data una stringa str, e due indici di posizione
   j e k, riportare la sottostringa di str che va dall'indice 
   j all'indice k, inclusi;
   la lunghezza della sottostringa e` (k-j)+1 *)
(* substring : string -> int -> int -> string *)
let substring str j k =
  String.sub str j ((k-j)+1)

(* qual e` l'ultima posizione in una stringa?
Nel modulo String:

val length : string -> int
Return the length (number of characters) of the given string.
*)

(* come convertire una stringa in int? *)
(* http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html 
   String conversion functions

val string_of_int : int -> string
Return the string representation of an integer, in decimal.

val int_of_string : string -> int
Convert the given string to an integer.
Raise Failure "int_of_string" if the given string is not a valid 
representation of an integer, 
*)

(*  split_string : string -> int * char * int *)
let split_string str =
  let i = primo_non_numerico str 
  in (int_of_string (substring str 0 (i-1)),
      str.[i],
      int_of_string (substring  str (i+1) ((String.length str)-1)))


(* problema principale
   evaluate: string -> int *)
let evaluate str =
  let (n,op,m) = split_string str
  in 
  if op='+' then n+m
  else if op='-' then n-m
  else if op='*' then n*m
  else n/m

(* Osservazione:
   let (n,op,m) = split_string str ...
   che succede nell'ambiente? *)

(* Eccezioni:
   String.sub "pippo" 3 20
   int_of_string "18.3"
*)

(* ------------------------- *)
(* Eccezioni *)
exception NegativeNumber
let rec fact n =
  if n<0 then raise NegativeNumber
  else if n=0 then 1
       else n * fact (n-1)

(* 05-ricorsione-2, pag 11-14 (22-25) *)
(* ------------------------- *)

exception BadOperation

(* evaluate: string -> int *)
let evaluate str =
  let (n,op,m) = split_string str
  in 
  if op='+' then n+m
  else if op='-' then n-m
  else if op='*' then n*m
  else if op='/' then n/m
  else raise BadOperation

(* evaluate "3&9"
   evaluate "3*pippo" : int_of_string in split_string
   evaluate "345"     : primo_non_numerico
*)

let primo_non_numerico str =
  (* aux: int -> int *)
  let rec aux i =
    if i = String.length str then raise BadOperation
    else 
      if not (numeric str.[i]) then i
      else aux (i+1)
  in aux 0

(* oppure: *)
let primo_non_numerico str =
  (* aux: int -> int *)
  let rec aux i =
      if not (numeric str.[i]) then i
      else aux (i+1)
  in 
  try aux 0
  with Invalid_argument "index out of bounds" -> raise BadOperation

(* Le eccezioni si possono "catturare"
   try E
   with Ex -> F
        E` un'espressione.
        Qual e` il suo tipo e il suo valore?
        Restrizioni sui tipi di E e F?
*)

exception BadInt

(*  split_string : string -> int * char * int *)
let split_string str =
  let i = primo_non_numerico str 
  in 
  try (int_of_string (substring str 0 (i-1)),
       str.[i],
       int_of_string (substring  str (i+1) ((String.length str)-1)))
  with  Failure "int_of_string" -> raise BadInt 

let evaluate str =
    let (n,op,m) = split_string str
    in 
    if op='+' then n+m
    else if op='-' then n-m
    else if op='*' then n*m
    else if op='/' then n/m
    else raise BadOperation

(* pattern matching: i casi di evaluate si possono distinguere
   a seconda del "pattern" del valore di split_string str *)

let evaluate str =
  match split_string str with
    (n,'+',m) ->  n+m
  | (n,'-',m) ->  n-m
  | (n,'*',m) ->  n*m
  | (n,'/',m) ->  n/m
  | x -> raise BadOperation

(* cosa succede senza l'ultimo caso? *)

(* variabile "muta" *)
let evaluate str =
  match split_string str with
    (n,'+',m) ->  n+m
  | (n,'-',m) ->  n-m
  | (n,'*',m) ->  n*m
  | (n,'/',m) ->  n/m
  | _ -> raise BadOperation

let primo_non_numerico str =
  (* aux: int -> int *)
  let rec aux i =
      if not (numeric str.[i]) then i
      else aux (i+1)
  in 
  try aux 0
  with _ -> raise BadOperation

let split_string str =
  let i = primo_non_numerico str 
  in 
  try (int_of_string (substring str 0 (i-1)),
       str.[i],
       int_of_string (substring  str (i+1) ((String.length str)-1)))
  with _ -> raise BadInt 

(* ------------- *)
(* fact definita mediante pattern matching *)
let rec fact n = 
  match n with
    0 -> 1
  | _ -> n * fact(n-1)

(* o anche: *)
let rec fact = function
    0 -> 1
  | n -> n * fact(n-1)

(* qual e' il tipo della funzione definita qui sotto? *)
let rec fact n = function
    0 -> 1
  | n -> n * fact(n-1)

(* si possono anche usare "pattern multipli": *)
let rec fact = function
    0 | 1 -> 1
  | n -> n * fact(n-1)

(* Cosa ci puo' stare in un pattern? *)
(* 06-pattern p. 2-6, 9--10, 15, 18-20 (26-37) *)
(* ------------------------- *)

(* gestione dell'ambiente nella chiamata di funzioni ricorsive
   Call by value e call by name, necessita' di espressioni lazy
   05-ricorsione-2, pag 23-25 (38-39) *)