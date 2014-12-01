(* Esercizio 1j *)
(* mapcons: ('a * 'b list) list -> 'b -> ('a * 'b list) list *)
(* questa funzione data una lista di coppie in cui il primo elemento *)
(* � un 'a ed il secondo � una 'b list che restituisce una lista dello*)
(* stesso tipo a cui ad ogni secondo elemento viene aggiunto in testa elem*)

let mapcons l elem = List.map (function (x,y) -> (x, elem::y)) l