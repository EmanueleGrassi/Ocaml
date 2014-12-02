(* Esercizio 8 *)
type expr =
  | Jolly
  | Int of int
  | Var of string
  | Sum of expr * expr
  | Diff of expr * expr
  | Mult of expr * expr
  | Div of expr * expr

let rec pattern_matching e1 e2 =
	match e1 with
	| Int n -> match e2 with
             | Jolly -> true
						 | _ -> false
	| Var s -> match e2 with
             | Jolly -> true
						 | _ -> false
	| Sum (a,b) -> match e2 with
                 | Jolly -> true
    						 | Sum(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								 | _ -> false 
  | Diff (a,b) -> match e2 with
                 | Jolly -> true
  							 | Diff(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								 | _ -> false
  | Mult (a,b) -> match e2 with
                 | Jolly -> true
  							 | Mult(aa,bb) -> pattern_matching a aa && pattern_matching b bb
                 | _ -> false
  | Div (a,b) -> match e2 with
                 | Jolly -> true
  							 | Div(aa,bb) -> pattern_matching a aa && pattern_matching b bb
								 | _ -> false 
 | _ -> failwith "NotAnExpression"