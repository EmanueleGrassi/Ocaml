(* Esercizio 2a *)
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