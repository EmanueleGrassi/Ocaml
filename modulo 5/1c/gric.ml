(* Esercizio 1c *)
(* split: ('a * 'b) list -> 'a list * 'b list *)
(* split è una funzione che presa una lista di coppie di valori *)
(* restituisce una coppia di liste*)
(* sono state fornite una versione ricorsiva ed una versione tail recursive*)
(* della stessa funzione.*)
(* La versione tail recursive utilizza la funzinone aux*) 
(* aux: 'a list * 'b list -> ('a * 'b) list -> 'a list * 'b list *)
(* nella quale tmp viene utilizzata per costruire il risultato finale *)

let split_coda lst =
  let rec aux tmp = function
    | [] -> ((List.rev (fst tmp)), (List.rev (snd tmp)))
    | (x, y) :: rest -> aux ((x :: (fst tmp)), (y :: (snd tmp))) rest
  in aux ([], []) lst
  
let rec split = function
  | [] -> ([], [])
  | (x, y) :: rest -> ((x :: (fst (split rest))), (y :: (snd (split rest))))
  
