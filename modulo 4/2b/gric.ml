(* Esercizio 2b *)
(* nondec è una funzione che verifica se gli elementi della lista sono in ordine *)
(* non decrescente *)
(* nondec: 'a list -> bool *)

let rec nondec = function
 | [] -> true
 | x::y::rest -> x <= y && nondec (y::rest)
 | x::rest -> true