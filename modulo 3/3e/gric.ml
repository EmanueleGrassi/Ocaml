(* Esercizio 3e *)
(* maxstring: string->char*)
(* questa funzione prende una stringa come parametro e ne restituisce il *)
(* carattere più grande (secondo la tabella ASCII)*)
(* se il parametro è una stringa vuota viene sollevata l'eccezione String_vuota *)

exception Stringa_vuota
let maxstring s =
	match s with
	|"" -> raise Stringa_vuota
	|_ -> let rec aux c i =
      		let maxi = max c s.[i]
      		in if(i < (String.length s) -1)
      			 then aux maxi (i+1)
      		   else c
				in aux s.[0] 1;;