(* Esercizio 5 *)
(* trips : 'a list -> ('a * 'a * 'a) list *)
(* è una funzione che data una lista restituisce una lista contenenete *)
(* tutte le triple di valori consecutivi presi dalla lista *)
(* passata come parametro *)
(* questa funzione utilizza aux *)
(* aux: ('a * 'a * 'a) list -> 'a list -> ('a * 'a * 'a) list *)
(* la lista tmp viene utilizzata per costruire il risultato. Ad ogni chiamata*)
(* ricorsiva viene individuata una tripla valida e viene aggiunta a tmp *)

let trips lst = 
	let rec aux tmp = function 
		| [] -> List.rev tmp
		| x::y::z::rest -> aux ((x,y,z)::tmp) (y::z::rest)
		| _::rest -> List.rev tmp
  in aux [] lst