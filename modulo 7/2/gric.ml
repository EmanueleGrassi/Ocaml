(* Esercizio 2*)
(* prodotto: nat -> nat -> nat*)
(* prodotto è la funzione che implementa la moltiplicazione mediante somma fra numeri*)
(* naturali rappresentati dal tipo 'nat' definito a lezione*)
(* La funzione somma: nat -> nat -> nat è quella che è stata definita a lezione *)

let rec prodotto n m =
	match m with
	| Zero -> Zero
	| Succ(Zero) -> n
	| Succ k -> somma n (prodotto n k)

