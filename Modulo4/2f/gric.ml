(* Esercizio 2f *)

exception Not_Found

let position elem xs =
	let rec aux n = function
	| [] -> raise Not_Found
	| x::rest -> if x = elem
							 then n
							 else aux (n+1) rest
  in aux 0 xs