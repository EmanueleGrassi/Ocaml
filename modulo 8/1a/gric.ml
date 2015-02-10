(* Esercizio 1a *)
(* subexpr: expr -> expr -> bool *)
(* subexpr e1 e2 determina se e2 è sottoespressione di e1*)

let rec subexpr e1 e2 = 
	match e1 with
	| Int n -> e1=e2
	| Var s -> e1=e2
	| Sum(a,b) | Diff(a,b) | Mult(a,b) | Div(a,b) -> e1=e2 || (subexpr a e2 || subexpr b e2)