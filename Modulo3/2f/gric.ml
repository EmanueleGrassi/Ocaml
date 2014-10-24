(* Esericizio 2f *)

let stringa_max () = 
	let tmp = read_line()
	in let rec aux s = 
		   let t = read_line()
    		 in match t with 
    		| "." -> s
    		| _ -> if String.length s > String.length t
    		       then aux s
    					 else aux t
		 in match tmp with
  	    | "." -> tmp
  	    | _ -> aux tmp