(* come si implementa un ciclo? *)
(* funzioni di input e output
   
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
   
Standard input e standard output:
val print_string : string -> unit
val print_int : int -> unit
....
val read_line : unit -> string
val read_int : unit -> int
...

sequenze di comandi
*)

(* ciclo: int -> int -> unit
   ciclo n m: stampa gli interi da n a m (estremi inclusi) *)
let rec ciclo n m =
  if n>m then ()
  else (print_int n;    (* sequenza di comandi *)
	print_newline();
	ciclo (n+1) m)

(* ciclo e' "tail recursive": al ritorno dalla chiamata ricorsiva non si 
   deve fare nulla *)

(* ciclo2: int -> unit 
   ciclo2 n: stampa degli interi da 0 a n *)
let ciclo2 = ciclo 0

(* leggi: unit -> int
   legge una sequenza di linee terminata da "." 
    e ne riporta il numero *)
let rec leggi () =
  let s = read_line ()
  in if s="." then 0
  else 1 + leggi()

(* leggi non e' tail recursive: al ritorno dalla ricorsione si deve ancora
   aggiungere 1 al risultato *)

(* somma: unit -> int
   legge una sequenza di interi terminata da "." e 
   riporta la somma dei numeri letti *)
let rec somma () =
  let s = read_line ()
  in if s="." then 0
  else (int_of_string s) + somma()

(* somma non e' tail recursive *)

(* uso "sporco" delle eccezioni *)
let rec somma2 () =
  try let n = int_of_string (read_line())
      in n + somma2()
  with _ -> 0

(*stringlen: string -> int *)
(* lunghezza di una stringa   =   String.length *)
let stringlen s =
  (* aux: int -> int
     aux i = numero di caratteri in s che vanno dalla posizione i
             alla fine della stringa.
     implementa un ciclo. E' tail recursive *)
  let rec aux i =
    try let _ = s.[i] (* serve solo per verificare se s.[i] esiste *)
        in aux (i+1)
    with _ -> i
  (* inizializzazione del ciclo: inizialmente, i=0 *)
  in aux 0

(* loop: unit -> int * int
   riporta numero e somma degli interi letti *)
let rec loop () =
  let s = read_line()
  in
  if s="."  (* terminato? *)
  then (0,0) (* nessun numero letto, somma 0 *)
  else (* stringa rappresenta un int, leggi gli altri numeri *)
    let (tot,somma) = loop()
    in (tot+1,somma+(int_of_string s))

(* oppure *)
let rec loop () =
  try let n = int_of_string(read_line())
      in let (tot,somma) = loop()
      in (tot+1,somma+n)
  with _ -> (0,0)

(* loop non e' tail recursive *)

(* General input/output functions:
   
http://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html

val open_out : string -> out_channel
val output_string : out_channel -> string -> unit
val close_out : out_channel -> unit
val open_in : string -> in_channel
val input_line : in_channel -> string
val close_in : in_channel -> unit
*)

(* obiettivo: leggere da file una sequenza di numeri *)
(* interi, terminata dalla stringa ".", scrivere il numero di interi *)
(* letti, la loro somma, e la media *)
(* media: string -> unit *)
(* la stringa e` il nome del file *)

let rec media file =
  (* apertura del canale di input *)
  let inchan = open_in file
  in (* "ciclo" di lettura dei numeri *)
     (* riporta numero di interi letti e loro somma *)
  (* loop: unit -> int * int *)
  let rec loop () =
    let stringa = input_line inchan (* lettura da file *)
    in 
    if stringa = "." 
    then  (* sequenza di comandi *)
      begin 
        close_in inchan; (* chiusura del canale di input *)
        (0,0)
      end
    else
      let (n,somma) = loop () 
      in (n+1, somma + (int_of_string stringa))
  in let (n,somma) = loop ()
  in   (* sequenza di comandi *)
  print_string ("Letti "^(string_of_int n)^
		" interi\nSomma: "^(string_of_int somma)^
		"\nMedia: "^
		(string_of_float ((float_of_int somma)/.(float_of_int n)))
	       ^"\n")
  (* niente overloading *)

(* ----------------------------- *)
(* processi ricorsivi e iterativi *)
let rec fact = function
    0 -> 1
  | n -> n * fact(n-1)

(* la funzione non e' tail recursive: implementa un processo ricorsivo *)

(* 
fact 3 ==> 3 * fact 2
       ==> 3 * (2 * fact 1)
       ==> 3 * (2 * (1 * fact 0))
       ==> 3 * (2 * (1 * 1))
       ==> 3 * (2 * 1)
       ==> 3 * 2
       ==> 6

Ma il prodotto e' associativo, quindi:
      3 * (2 * fact 1) = (3 * 2) * fact 1
e:
    fact 3 = 3 * fact 2
          = (3 * 2) * fact 1
          = (6 * 1) * fact 0
          = 6 * 1
          = 6
Non c'e' bisogno di aspettare il risultato delle chiamate ricorsive,
possiamo eseguire subito il calcolo 3*2 e conservarlo in un 
"accumulatore" (o risultato parziale)
              ==> algoritmo iterativo *)

let rec fact' n =
  (* aux: int -> int -> int
     il primo argomento e' il "risultato parziale" *)
  let rec aux f = function 
      0 -> f   (* il "ciclo" termina *)
    | n -> aux (f*n) (n-1)
  in aux 1 n

(* il ciclo e' implementato mediante un costrutto ricorsivo.
   Uso di una funzione ausiliaria che ha un parametro in piu', 
   l'accumulatore: i suoi argomenti sono le variabili che vengono
   "modificate" nel ciclo.
   La funzione principale richiama quella ausiliaria "inizializzando"
   le variabili del ciclo *)

(* il processo e' iterativo:
   fact' 3 = aux 3 1
           = aux(2,3)
           = aux(1,6)
           = aux(0,6)
           = 6
   Dopo aver raccolto il risultato della chiamata
   ricorsiva, non si deve fare nulla. *)

(* Ma cosa calcola aux (specifica dichiarativa) ? *)

(* Un processo ricorsivo:
   - esegue calcoli al ritorno dalla ricorsione
   - usa spazio proporzionale alla "dimensione" dell'input.
  In un processo iterativo:
   - il risultato parziale viene conservato in un accumulatore;
   - dopo aver faccolto il risultato della chiamata ricorsiva non si
     deve fare nulla
   - l'ultima chiamata puo' riportare il suo risultato direttamente
     alla prima
*)

(* ciclo implementa un processo iterativo o ricorsivo?
   leggi, somma, stringlen, loop ? *)

(* leggi_it: unit -> int
   legge una sequenza di linee terminata da "." 
    e ne riporta il numero *)
let leggi_it () =
  let rec aux n =  (* n = numero di righe lette finora *)
    let s = read_line()
    in if s="." then n    (* n e non 0 *)
    else aux (n+1) (* incremento del risultato parziale *)
          (* i calcoli sono eseguiti PRIMA della chiamata ricorsiva *)
  in aux 0 (* inizializzazione del risultato parziale *)

(* somma_it: unit -> int
   legge una sequenza di interi terminata da "." e 
   riporta la somma dei numeri letti *)
let somma_it () =
  let rec aux tot =  (* tot = somma dei numeri letti finora *)
    let s = read_line()
    in if s="." then tot    (* tot e non 0 *)
    else aux (tot + (int_of_string s)) (* incremento del risultato parziale *)
  in aux 0 (* inizializzazione del risultato parziale *)

(* loop_it: unit -> int * int
   riporta numero e somma degli interi letti *)
let loop_it () =
  (* aux: int -> int -> int * int *)
  let rec aux tot somma = (* due "accumulatori" *)
    let s = read_line ()
    in if s="." then (tot,somma)  (* e non (0,0) *)
    else aux (tot+1) (somma + (int_of_string s))
              (* incremento degli "accumulatori" *)
  in aux 0 0 

(* uso di loop iterativo in media *)
(* media_it: string -> unit *)
(* media_it s = legge dal file di nome s una sequenza di numeri *)
(* interi, terminata dalla stringa ".", e stampa il numero di interi *)
(* letti, la loro somma, e la media *) 
let rec media_it file =
  let inchan = open_in file in 
  let rec loop tot somma = 
    let s = input_line inchan
    in 
    if s = "." 
    then 
      begin 
	close_in inchan; 
	(tot,somma)
      end 
    else loop (tot+1) (somma + (int_of_string s))
  in let (n,somma) = loop 0 0 (* <=== inizializzazione *)
  in 
  print_string ("Letti "^(string_of_int n)^
		" interi\nSomma: "^(string_of_int somma)^
		"\nMedia: "^
		(string_of_float ((float_of_int somma)/.(float_of_int n)))
	       ^"\n")