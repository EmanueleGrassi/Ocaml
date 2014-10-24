(* Esercizio 2b *)
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