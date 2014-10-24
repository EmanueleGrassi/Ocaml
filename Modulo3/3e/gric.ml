(* Esercizio 3e *)
(* da finire, manca l'eccezione *)

let maxstring s =
	let rec aux c i =
		let maxi = max c s.[i]
			in if(i < (String.length s) -1)
				 then aux maxi (i+1)
				 else c
	in aux s.[0] 1