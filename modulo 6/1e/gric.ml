(* Esercizio 1e *)
(* pairwith: 'a -> 'b list -> ('a * 'b) list *)
(* Questa funzione dato un elemento ed una lista costruisce*)
(* una lista di coppie nelle quali il primo elemento è sempre elem*)
(* (parametro di pairwith) ed il secondo elemento sono tutti gli *)
(* elementi di lst *)

let pairwith elem lst = List.map (function x -> (elem,x)) lst