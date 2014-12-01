(* Esercizio 1a *)
(* find: ('a -> bool) -> 'a list -> 'a *)
(* find è una funzione che restituisce il primo elemento di una lista*)
(* che soddisfa il predicato p passato come parametro.*)
(* La funzione solleva l'eccezione Condition_not_Satisfied *)
(* se nessun elemento della lista soddisfa il predicato p.*)
(* find_applicata: int list -> int*)
(* è una funzione che utilizza find con un predicato opportuno per *)
(* riportare il primo elemento di una lista il cui quadrato è minore di 30 *)

exception Condition_not_Satisfied

let rec find p = function
	| [] -> raise Condition_not_Satisfied
	| x::rest -> if p x
               then x
							 else find p rest

									
let find_applicata lst = find (function x -> x*x < 30) lst

