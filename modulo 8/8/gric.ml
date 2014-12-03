(* Esercizio 8 *)
type expr =
   Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec pattern_matching e1 e2 =
	match e1 with
	| Int n -> (match e2 with
	            | Jolly -> true
							| Int nn -> n = nn
							| _ -> false)
	| Var s -> (match e2 with
	            | Jolly -> true
							| Var ss -> s = ss
							| _ -> false)
	| Sum(a,b) -> (match e2 with
	              | Sum(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								| Jolly -> true
								| _ -> false)
	| Diff(a,b) -> (match e2 with
	              | Diff(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								| Jolly -> true
								| _ -> false)
	| Mult(a,b) -> (match e2 with
	              | Mult(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								| Jolly -> true
								| _ -> false)
	| Div(a,b) -> (match e2 with
	              | Div(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								| Jolly -> true
								| _ -> false)
	| Jolly -> failwith "errore"