(* Esercizio 5 *)
(* most_general_match: 'a list -> 'a list -> 'a pattern list *)
(* è una funzione che passate due liste come parametro restituisce la lista di pattern*)
(* col minor numero di Jolly possibili conformi alle due liste.*)
(* La funzione solleva l'eccezione Lunghezze_non_uguali se le due liste hanno lunghezza *)
(* differente(questo viene riscontrato dal fallimento della funzione List.map2) *)

exception Lunghezze_non_uguali

type 'a pattern = Jolly | Val of 'a

let most_general_match l1 l2 =
  try List.map2 ( fun h1 h2 -> if (Val h1) = (Val h2) 
	                             then Val h1 
															 else Jolly ) l1 l2
	with Invalid_argument "List.map2" -> raise Lunghezze_non_uguali