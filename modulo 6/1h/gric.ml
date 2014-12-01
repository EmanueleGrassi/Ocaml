(* Esercizio 1h *)
(* subset: 'a list -> 'a list -> bool*)
(* è una funzione che verifica che tutti gli elementi di lst2*)
(* siano contenuti in lst*)

let subset lst lst2 = List.for_all ( function x -> List.mem x lst ) lst2