(* Secondo Esercizio*)
(* bello è una funzione che dato un intero n restituisce true se:*)
(* caso1 - se il numero è ad una cifra verifica che è 0,3 o 7 (quindi bello) *)
(* caso2 - se il numero è a più cifre verifica *)
(* che la sua ultima cifra è un numero bello e la penultima non lo è. *)
(* bello: int->bool *)
(* DESCRIZIONE: *)
(* all'interno della funzione bello sono state definite alcune funzioni di supporto:*)
(* ultime_cifre: int->int*int *)
(* ultime_cifre è una funzione che dato un intero restituisce una coppia *)
(* contenente le ultime due cifre dell'intero dato *)
(* isbello: int->bool *)
(* isbello verifica che l'intero passato come parametro sia 0,3 o 7. Quindi bello *)


let bello n =
  let ultime_cifre n = let m = abs n in (((m / 10) mod 10), (m mod 10)) 
	in let isbello n = match n with 
	  							   | 0 | 3 | 7 -> true 
	                   | _ -> false
     in if n < 10
        then isbello n
    	  else let (p, u) = ultime_cifre n 
				     in (not (isbello p)) && (isbello u);;
  
