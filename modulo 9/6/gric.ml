(* Esercizio 6 *)
(* duale: form -> form*)
(* è una funzione che data una formula restituisce la sua duale.*)
(* Se la formula non è in forma normale negativa, viene sollevata l'eccezione*)
(* Not_NNF.*)
(* Questa funzione utilizza le funzioni test_nnf e complementare, definite*)
(* negli esercizi precedenti e della funzione aux*)
(* aux: form -> form*)
(* che prende come parametro una formula in forma normale negativa e elabora*)
(* la sua duale invertendo gli And con gli Or ed applicando la funzione*)
(* complementare agli atomi *)


exception Not_NNF

let duale f =
	if not(test_nnf f)
	then raise Not_NNF
	else let rec aux = function
  			| And (p,q) -> Or(aux p, aux q)
  			| Or (p,q) -> And(aux p, aux q)
  			| r -> complementare r
			 in aux f