(* Esercizio 8 *)
(* pattern_matching: expr -> expr -> bool *)
(* pattern_matching è una funzione che data un'espressione e ed un modello m*)
(* ritorna true se e ed m hanno la stessa struttura ovvero se è possibile*)
(* ottenere e da m sostituendo i Jolly con opportune expr *)

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