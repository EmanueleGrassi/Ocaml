(* Esercizio 2b *)

let rec nondec = function
 | [] -> true
 | x::y::rest -> x <= y && nondec (y::rest)
 | x::rest -> true


(*let rec nodc = function
|[] -> true 
|[x]-> true
|x::rest -> x <= List.hd rest && nodc rest;*)