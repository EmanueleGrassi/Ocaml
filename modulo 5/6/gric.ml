(* Esercizio 6 *)
(* choose : int -> 'a list -> 'a list list *)
(* choose preso un un intero n ed una lista, restituisce una lista *)
(* contenente tutte le sottosequenze della lista di lunghezza n*)
(* choose utilizza della funzione take definita a lezione e riportata*)
(* in questo file e di aux*)
(* aux: 'a list list -> 'a list -> 'a list list*)
(* in aux la lista tmp viene usata per costruire il risultato.*)
(* Ad ogni chiamata ricorsiva, se la lista ha una lunghezza maggiore di n,*)
(* allora viene cercata una sottosequenza e viene aggiunta a tmp *)

let rec take n =
  function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: (take (n - 1) xs)

let choose n lst =
	let rec aux tmp = function
		| [] -> List.rev tmp
		| x::rest as xlst -> if (List.length xlst < n)
												 then aux tmp rest
												 else aux ((take n xlst)::tmp) rest
	in aux [] lst