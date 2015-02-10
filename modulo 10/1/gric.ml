(* Esercizio 1 *)
(* subexpr: multi_expr -> multi_expr -> bool *)
(* è una funzione che date due espressioni e1 e2 verifica che e2 sia una*)
(* sottoespressione di e1 *)
(* subst: multi_expr -> string -> multi_expr -> multi_expr *)
(* è una funzione che data una espressione e1, una stringa s, ed un'espressione e2*)
(* sostituisce ogni occorrenza di Multivar s in e1 con e2 *)

let rec subexpr e1 e2 = 
	match e1 with
	| MultiInt _ -> e1 = e2
 	| MultiVar _ -> e1 = e2
  | MultiDiff (n,g) -> e1 = e2 || (subexpr n e2 || subexpr g e2)
	| MultiDiv (n,g) -> e1 = e2 || (subexpr n e2 || subexpr g e2)
	| MultiSum l -> e1 = e2 || (List.exists (function x -> subexpr x e2) l)
	| MultiMult l -> e1 = e2 || (List.exists (function x -> subexpr x e2) l)


let rec subst e1 s e2 =
	match e1 with
	| MultiVar p -> if p = s then e2 else e1
	| MultiInt n -> e1
	| MultiSum l -> MultiSum (List.map (function x -> subst x s e2) l)
	| MultiDiff(a,b) -> MultiDiff((subst a s e2),(subst b s e2))
	| MultiMult l -> MultiMult(List.map (function x -> subst x s e2) l)
	| MultiDiv(a,b) -> MultiDiv((subst a s e2),(subst b s e2))