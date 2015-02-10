(* Problema: attraversamento di un labirinto "quadrato", da una casella
   di entrata a una casella di uscita, senza passare per caselle
   che contengono un mostro. 
   Ci si puo' spostare in orizzontale e in diagonale, ma solo verso
   destra. La casella di ingresso e' nella colonna piu' a sinistra. *)
(**  in modo da non avere necessita' di loop-checking **)

(*        0  1  2  3  4
       0        M
       1     M     M
       2           M  
       3  M        
       4        M  
*)

(* Rappresentazione:  
   - dimensione della matrice
   - lista con le caselle contenenti un mostro
Esempio:
*)
let dim = 5
let con_mostro = [(0,2);(1,1);(1,3);(2,3);(3,0);(4,2)]

(* Un caso di fallimento: siamo fuori matrice *)
(* sottoproblema: dato l'indice massimo della matrice e una casella 
    (riga,colonna), determinare se la casella e` nella matrice *)
(*  in_labirinto : int -> int * int -> bool *)
let in_labirinto dim (r,c) =
  r >= 0 && c >= 0 && r < dim && c < dim

(* altro caso di fallimento: la casella contiene un mostro *)
(* basta verificare - con List.mem - se la casella e' un elemento
   della lista delle caselle con mostro *)
(*  has_monster : 'a -> 'a list -> bool *)
let has_monster = List.mem

(* Ricerca del percorso, versione 1 *)
(* path1: int -> (int * int) list -> 
               int * int -> int * int -> (int * int) list *)
(* path1 dim mostri ingresso uscita riporta - se esiste -
   un cammino (lista di caselle) nella matrice di dimensione dim
   che va da ingresso a uscita senza passare da caselle con il mostro
   (caselle nella lista mostri) *)
(* ricerca mediante backtracking *)

(* per segnalare il fallimento *)
exception NotFound 

let path1 dim mostri ingresso uscita =
  (* cerca: int * int  -> (int * int) list *)
  let rec cerca_da ((r,c) as casella) =
      (* casella e` fuori della matrice o contiene un mostro? *)
    if not (in_labirinto dim casella)
         || List.mem casella mostri
    then raise NotFound
    else (* siamo arrivati? *)
      if casella = uscita 
      then [casella] (* il cammino contiene solo la casella corrente *)
      else (* passiamo per casella e proseguiamo con 
		  una delle caselle accessibili *)
	casella:: (* il cammino inizia con la casella corrente, e poi ... *)
	try cerca_da (r,c+1)
	with NotFound ->
	  try cerca_da (r+1,c+1)
	  with NotFound -> 
	    cerca_da (r-1,c+1)
  in cerca_da ingresso

(**
# path1 5 con_mostro (1,0) (1,4);;
- : (int * int) list = [(1, 0); (2, 1); (1, 2); (0, 3); (1, 4)]
# path1 5 con_mostro (0,0) (4,4);;
Exception: NotFound.
**)

(* Seconda versione:
   andiamo verso una generalizzazione al caso in cui una casella possa 
   avere un numero qualsiasi di vicini.
   I vicini sono dunque rappresentati da una lista di caselle.
   Possiamo allora gia' "filtrarla" e conservare solo le caselle
   interne al labirinto *)
(* sottoproblema: "filtrare" una lista di caselle conservando solo
   quelle interne al labirinto *)
(* filter_vicini: int -> (int * int) list -> (int * int) list *)
let rec filter_vicini dim = function
    [] -> []
  | casella::rest ->
      if in_labirinto dim casella
      then casella::filter_vicini dim rest
      else filter_vicini dim rest

(* sottoproblema: data una casella e la struttura di un labirinto, 
   trovare le caselle interne al labirinto ad essa accessibili.  
   Se la casella non e` nel 
   labirinto, sollevare un'eccezione *)
(* vicini : int -> int * int -> (int * int) list *)
let vicini dim (r,c) =
  if in_labirinto dim (r,c)
  then filter_vicini dim [(r,c+1);(r+1,c+1);(r-1,c+1)]
  else raise NotFound

(* Per usare la lista dei vicini, gia` "filtrata"
   eliminando le caselle che non sono nella matrice, dobbiamo
   generalizzare l'espressione:
	    try cerca_da (r,c+1) 
	    with NotFound ->
	      try cerca_da (r+1,c+1) 
	      with NotFound -> 
		 cerca_da (r-1,c+1)
   in modo che la ricerca possa proseguire a partire da una
   qualsiasi delle caselle in una lista data

   cerca_da si applica a una casella, non puo` applicarsi a una
   lista di caselle.  Dovremmo definire una funzione analoga a
   cerca_da che pero` si applichi a liste di caselle e riportare
       casella::cerca_da_lista (vicini dim casella)

   cerca_da_lista deve richiamare cerca_da per ogni elemento della 
   lista.  Quindi:
       cerca_da       usa   cerca_da_lista
       cerca_da_lista usa   cerca_da
   cerca_da_lista lst = un cammino fino all'uscita a partire da una
                        qualsiasi casella in lst
*)
(* ----------- *)
(* MUTUA RICORSIONE
      f chiama g, e g chiama f *)
(* pari/dispari : int -> bool
      definite solo sui naturali *)
let rec pari n = 
  n<>1 && (n=0 or dispari (n-1))
and dispari n =
  n<>0 && (n=1 or pari (n-1));;
(* ----------- *)

(* ora assumiamo dunque che le caselle visitate siano sicuramente
   interne al labirinto *)
let path2 dim mostri ingresso uscita =
  (* cerca: int * int -> (int * int) list -> (int * int) list *)
  let rec cerca_da casella  =
      (* il primo caso di fallimento sparisce *)
    if List.mem casella mostri
    then raise NotFound
    else 
      if casella = uscita 
      then [casella] 
      else casella::cerca_da_lista (vicini dim casella) 
  and cerca_da_lista caselle  =
    match caselle with
      [] ->  raise NotFound
    | c::rest -> (* provo a passare per c, ma se fallisco
                    provo ancora con una delle caselle di rest *)
	try cerca_da c 
	with NotFound -> cerca_da_lista rest 
  in cerca_da ingresso 

(**=====================================**)
(* cosa succederebbe se non si fosse sempre obbligati a spostarsi a*)
(* destra, ma ci si potesse spostare in orizzontale, verticale e*)
(* diagonale in tutte le direzioni? *)

(* int -> int * int -> (int * int) list *)
(* vicini2 dim casella = vicini di casella che stanno nel labirinto *)
let vicini2 dim (r,c) =
  if in_labirinto dim (r,c)
  then filter_vicini dim [(r,c+1);(r+1,c+1);(r-1,c+1); (* a destra *)
		          (r-1,c);(r+1,c); (*su o giu *)
		          (r,c-1);(r+1,c-1);(r-1,c-1)] (* a sinistra *)
  else raise NotFound

let path3 dim mostri ingresso uscita =
  (* cerca: int * int -> (int * int) list -> (int * int) list *)
  let rec cerca_da casella  =
    if List.mem casella mostri
    then raise NotFound
    else 
      if casella = uscita 
      then [casella] 
      else casella::cerca_da_lista (vicini2 dim casella)
		     (** usiamo vicini2 invece di vicini **)
  and cerca_da_lista caselle  =
    match caselle with
      [] -> raise NotFound
    | c::rest -> 
	try cerca_da c 
	with NotFound -> cerca_da_lista rest 
  in cerca_da ingresso 

(*
# path3 5 con_mostro (0,0) (4,4);;
Stack overflow during evaluation (looping recursion?).
*)

(* dobbiamo tenere traccia delle caselle gia` visitate, per
   evitare di passarci di nuovo *)

let path4 dim mostri ingresso uscita =
  (* cerca: int * int -> (int * int) list -> (int * int) list *)
  let rec cerca_da casella visited =
         (* parametro in piu': lista delle caselle gia' visitate *)
    if List.mem casella mostri
       || List.mem casella visited (* casella gia' visitata *)
    then raise NotFound
    else 
      if casella = uscita 
      then [casella]
      else casella::cerca_da_lista (vicini2 dim casella)
		      (casella::visited) (* aggiungicamo casella
					    ai visitati *)
  and cerca_da_lista caselle visited = (* parametro in piu' *)
    match caselle with
      [] -> raise NotFound
    | c::rest -> 
	try cerca_da c visited (* parametro in piu' *)
	with NotFound -> cerca_da_lista rest visited
                         (* parametro in piu' *)
  in cerca_da ingresso [] (* parametro in piu' *)

(*
# path4 5 con_mostro (0,0) (4,4);;
- : (int * int) list =
[(0, 0); (0, 1); (1, 2); (0, 3); (0, 4); (1, 4); (2, 4); (3, 4); (4, 4)]
*)