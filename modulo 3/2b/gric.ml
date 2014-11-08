(* Esercizio 2b *)
(* read_max_min: unit -> int*int *)
(* questa funzione legge da input una sequenza di numeri terminata *)
(* da una stringa non numerica e restituisce una coppia i cui *)
(* elementi sono il massimo ed il minimo della sequenza immessa *)
(* Se non viene immesso nessun numero viene sollevata l'eccezione *)
(* NumeroNonImmesso *)
exception NumeroNonImmesso

let read_max_min () =
	let massimoTmp = 
		 try (int_of_string(read_line()), int_of_string(read_line()))
		 with _ -> raise NumeroNonImmesso
  in let rec aux (massimo, minimo) =  
    			try let s = int_of_string(read_line())
							in aux (max s massimo, min s minimo)
    			with _ -> (massimo, minimo)
		  in aux massimoTmp