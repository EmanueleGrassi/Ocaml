(* Esercizio 1g*)
(* setdiff: 'a list -> 'a list -> 'a list*)
(* è una funzione che elimina dalla prima lista gli elementi*)
(* presenti nella seconda lista *)

let setdiff lst lst2 = List.filter (function x -> not (List.mem x lst2) ) lst