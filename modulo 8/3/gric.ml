(* Esercizio 3 *)
(* foglie_in_lista: 'a list -> 'a tree -> bool *)
(* foglie_in_lista lst tree verifica che ogni foglia di tree sia contenuta in lst*)

let rec foglie_in_lista lst = function
  	| Empty -> false
  	| Tr(x,Empty,Empty) -> List.mem x lst
		| Tr(_,Empty,b) -> foglie_in_lista lst b
		| Tr(_,a,Empty) -> foglie_in_lista lst a
  	| Tr(_,a,b) -> foglie_in_lista lst a && foglie_in_lista lst b