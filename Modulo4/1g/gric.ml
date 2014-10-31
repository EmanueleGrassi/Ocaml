(* Esercizio 1g *)

exception Argomento_non_Valido

let nth n lst =
	if n < 0 || n > (List.length lst)-1
	then raise Argomento_non_Valido
	else let rec aux m l = 
      		if m = n 
      		then List.hd l
      		else aux (m+1) (List.tl l)
			 in aux 0 lst