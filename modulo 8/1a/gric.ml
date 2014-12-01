(* Esercizio 1a *)

type expr =
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec subexpr e1 e2 = 
	match e1 with
	| Int n -> e1=e2
	| Var s -> e1=e2
	| Sum(a,b) | Diff(a,b) | Mult(a,b) | Div(a,b) -> e1=e2 || (subexpr a e2 || subexpr b e2)