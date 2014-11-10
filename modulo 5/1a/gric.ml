(* Esercizio 1a *)
(* filter_vicini_coda è la versione tail recursive della funzione *)
(* filter_vicini definita in classe *)
(* filter_vicini_coda: int -> (int * int) list -> (int * int) list *)
(* questa funzione usa la funzione *)
(* aux: (int * int) list -> (int * int) list -> (int * int) list *)
(* che ha come parametri la lista tmp che verrà utilizzata *)
(* per costruire il risultato e una lista lst sulla quale*)
(* viene effettuata la ricerca degli elementi vicini *)

(* raccolti_coda è la versione tail recursive della funzione *)
(* raccolti definita in classe *)
(* raccolti_coda: int -> (int * int) list -> (int * int) list *)
(* questa funzione usa la funzione *)
(* aux: 'a list -> 'b list -> 'a list*)
(* che ha come parametri la lista tmp che verrà utilizzata *)
(* per costruire il risultato e una lista lst ('b list) *)
(* che viene scandita per trovare tutte le 'a degli elementi 'b di lst che sono *)
(* presenti nella lista contenuti (formata da coppie ('a * 'b list) *)
(* NB: le funzioni in labirinto e find_content sono state definite in classe*)
(* e vengono riportate in questo file affinchè compili correttamente*)

let in_labirinto dim (r, c) =
  (r >= 0) && ((c >= 0) && ((r < dim) && (c < dim)))
  
let rec find_content contenuti c =
  match contenuti with
  | [] -> []
  | (x, caselle) :: rest ->
      if List.mem c caselle
      then x :: (find_content rest c)
      else find_content rest c
  
let filter_vicini_coda dim lst =
  let rec aux tmp =
    function
    | [] -> List.rev tmp
    | casella :: rest ->
        if in_labirinto dim casella
        then aux (casella :: tmp) rest
        else aux tmp rest
  in aux [] lst
  
let raccolti_coda contenuti lst =
  let rec aux tmp =
    function
    | [] -> tmp
    | casella :: rest ->
        aux (List.append (find_content contenuti casella) tmp) rest
  in aux [] lst
  
