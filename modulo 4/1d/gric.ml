(* Esercizio 1d *)
(* drop è una funzione che data un intero ed una lista restituisce una lista*)
(* uguale alla prima esclusi i primi n elementi *)
(* drop: int->'a list->'a list*)
(* restituisce una lista vuota se n è maggiore o uguale alla lunghezza della lista *)
let rec drop n lst =
  try if n = 0 
		  then lst 
			else drop (n - 1) (List.tl lst)
  with Failure "tl" -> []
  
