(* Esercizio 1b *)

(*combine: 'a list -> 'b list -> ('a * 'b) list*)

exception Error

let combine lst1 lst2 = 
	if List.length lst1 <> List.length lst2
	then raise Error
	else let rec aux tmp lst1 lst2=
		try aux (((List.hd lst1), (List.hd lst2))::tmp) (List.tl lst1) (List.tl lst2)
		with Failure "tl" -> List.rev tmp
		   in aux [] lst1 lst2
	 

