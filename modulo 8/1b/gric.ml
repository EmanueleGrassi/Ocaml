(* Esercizio 1b *)
(* subst_in_expr: expr -> string -> expr -> expr *)
(* subst_in_expr e1 s e2 sostituisce ogni occorrenza di s in e1 con e2 *)

let rec subst_in_expr e1 s e2 =
	match e1 with
	| Var p -> if p = s then e2 else e1
	| Int n -> e1
	| Sum(a,b) -> Sum((subst_in_expr a s e2),(subst_in_expr b s e2))
	| Diff(a,b) -> Diff((subst_in_expr a s e2),(subst_in_expr b s e2))
	| Mult(a,b) -> Mult((subst_in_expr a s e2),(subst_in_expr b s e2))
	| Div(a,b) -> Div((subst_in_expr a s e2),(subst_in_expr b s e2))