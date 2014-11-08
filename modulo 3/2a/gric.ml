(* Esercizio 2a *)
(* read_max: unit -> int*)
(* questa funzione legge da input una sequenza di numeri terminata *)
(* da una stringa non numerica e restituisce il massimo *)
(* Se non viene immesso nessun numero viene sollevata l'eccezione *)
(* NumeroNonImmesso *)

exception NumeroNonImmesso

let read_max () =
	let massimoTmp = 
		 try int_of_string(read_line())
		 with _ -> raise NumeroNonImmesso
  in let rec aux massimo =  
    			try let s = int_of_string(read_line())
							in aux (max s massimo)
    			with _ -> massimo
		  in aux massimoTmp